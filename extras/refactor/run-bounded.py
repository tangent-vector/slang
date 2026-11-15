#!/usr/bin/env python3
# SPDX-FileCopyrightText: The Khronos Group, Inc.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
"""Run an unattended command with a deadline on Windows.

Example: python.exe extras/refactor/run-bounded.py --timeout 60 --log run.log -- slangc.exe -help

The command receives closed stdin and inherits stdout/stderr unless `--log` names
a file to replace with their combined output. We launch an executable directly;
batch files are rejected because Windows otherwise interprets them through a shell.
Exit statuses are 0 for success, 1 for command failure, 124 for timeout, 125 for
wrapper failure, and 130 for interruption. Invalid options produce status 2.

We start a private launcher that waits for permission before starting the command.
We first assign that launcher to a Windows Job Object, then release the launcher.
Every ordinary descendant inherits membership in the job. Closing our private job
terminates its remaining processes, including descendants left after a normal exit.
The job handle is not inherited, so Windows also closes it if this wrapper crashes.

We suppress inherited Windows system error dialogs without changing assertion
settings. Programs can still display their own dialogs or reset their error mode;
the deadline bounds those waits too. Slang builds should use `SLANG_IGNORE_ABORT_MSG=ON`.
Processes started through an external service, such as WMI, are outside the job.
Process creation and OS termination can add time beyond the requested deadline.
"""

import argparse
import ctypes
from ctypes import wintypes
import math
from pathlib import Path
import signal
import subprocess
import sys
import time


SEM_FAILCRITICALERRORS = 0x0001
SEM_NOGPFAULTERRORBOX = 0x0002
SEM_NOOPENFILEERRORBOX = 0x8000
JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE = 0x2000
JOB_OBJECT_EXTENDED_LIMIT_INFORMATION = 9
PROCESS_TERMINATE = 0x0001
PROCESS_SET_QUOTA = 0x0100
INTERRUPT_CHECK_INTERVAL_SECONDS = 0.2


def main():
    """Parse the command and run it with private process ownership and a deadline."""
    parser = argparse.ArgumentParser(
        usage="%(prog)s --timeout SECONDS [--cwd DIR] [--log FILE] -- COMMAND [ARG ...]",
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument("--timeout", required=True, type=float, help="deadline in seconds")
    parser.add_argument("--cwd", type=Path, help="working directory for the command")
    parser.add_argument("--log", type=Path, help="replace this file with stdout and stderr")

    # We parse only the wrapper's options, leaving every command argument intact.
    arguments = sys.argv[1:]
    separator = arguments.index("--") if "--" in arguments else len(arguments)
    options = parser.parse_args(arguments[:separator])
    command = arguments[separator + 1 :]
    if not command:
        parser.error("a command is required after --")
    if not math.isfinite(options.timeout):
        parser.error("--timeout must be finite")
    if options.timeout <= 0:
        parser.error("--timeout must be positive")
    if sys.platform != "win32":
        parser.error("this helper currently supports Windows only")
    if Path(command[0]).suffix.lower() in (".bat", ".cmd"):
        parser.error("batch files require a shell; name an executable instead")

    # We handle interruption in the owner so closing its job kills the entire tree.
    for interruption_signal in (signal.SIGINT, signal.SIGTERM, signal.SIGBREAK):
        signal.signal(interruption_signal, raise_interruption)
    try:
        suppress_windows_error_dialogs()
        if options.log is None:
            return run_bounded(command, options.timeout, options.cwd, None)
        with options.log.open("wb") as log:
            return run_bounded(command, options.timeout, options.cwd, log)
    except KeyboardInterrupt:
        print("run-bounded: interrupted", file=sys.stderr)
        return 130
    except (OSError, subprocess.SubprocessError) as error:
        print(f"run-bounded: {error}", file=sys.stderr)
        return 125


def run_bounded(command, timeout, cwd, log):
    """Start a gated launcher, contain it in a job, and wait until its deadline.

    We close the job on every exit path. For example, when a test process spawns
    a server and then exits, we also terminate the server when closing the job.
    """
    job = WindowsJob()
    launcher = None
    deadline = time.monotonic() + timeout
    try:
        # The launcher blocks on stdin before it can create any descendants.
        launcher_command = [sys.executable, str(Path(__file__).resolve()), "--_launcher"]
        launcher = subprocess.Popen(
            launcher_command + command,
            cwd=cwd,
            stdin=subprocess.PIPE,
            stdout=log,
            stderr=subprocess.STDOUT if log is not None else None,
            creationflags=subprocess.CREATE_NO_WINDOW,
        )
        job.assign(launcher.pid)
        with launcher.stdin:
            launcher.stdin.write(b"\n")

        # Direct file/parent handles avoid pipe readers that could wait on a server.
        # Windows process waits do not dispatch Python signal handlers. We return
        # to Python frequently so an interrupt can close the job promptly.
        while True:
            remaining = max(0, deadline - time.monotonic())
            try:
                return launcher.wait(timeout=min(remaining, INTERRUPT_CHECK_INTERVAL_SECONDS))
            except subprocess.TimeoutExpired:
                if time.monotonic() >= deadline:
                    print(f"run-bounded: timed out after {timeout:g} seconds", file=sys.stderr)
                    return 124
    finally:
        # Repeated Ctrl-C cannot interrupt the process-tree cleanup itself.
        for interruption_signal in (signal.SIGINT, signal.SIGTERM, signal.SIGBREAK):
            signal.signal(interruption_signal, signal.SIG_IGN)
        try:
            job.close()
        finally:
            if launcher is not None:
                # A failed job assignment leaves only the gated launcher to kill.
                launcher.kill()
                launcher.wait(timeout=5)
                if not launcher.stdin.closed:
                    launcher.stdin.close()


def run_launcher(command):
    """Wait for job assignment, then run the exact command with closed stdin.

    EOF means the owner failed before granting permission, so no command starts.
    We normalize child failures to 1 and print the original code so that a child
    exiting with 124 cannot be confused with the wrapper's deadline status.
    """
    if sys.stdin.buffer.read(1) != b"\n":
        return 125
    try:
        result = subprocess.run(
            command, stdin=subprocess.DEVNULL, creationflags=subprocess.CREATE_NO_WINDOW
        )
    except OSError as error:
        print(f"run-bounded: cannot start command: {error}", file=sys.stderr)
        return 125
    if result.returncode != 0:
        print(f"run-bounded: command exited with {result.returncode}", file=sys.stderr)
        return 1
    return 0


def raise_interruption(signum, frame):
    """Convert a handled signal to the exception that triggers job cleanup."""
    raise KeyboardInterrupt


def bind_windows_function(library, name, result_type, argument_types):
    """Describe a Win32 function's ABI, including pointer-sized handles."""
    function = getattr(library, name)
    function.restype = result_type
    function.argtypes = argument_types
    return function


def suppress_windows_error_dialogs():
    """Suppress inheritable OS error dialogs without disabling assertion checks."""
    kernel = ctypes.WinDLL("kernel32", use_last_error=True)
    get_mode = bind_windows_function(kernel, "GetErrorMode", wintypes.UINT, [])
    set_mode = bind_windows_function(kernel, "SetErrorMode", wintypes.UINT, [wintypes.UINT])
    # We preserve inherited flags and add critical-error, crash, and file-error suppression.
    suppressed_errors = SEM_FAILCRITICALERRORS | SEM_NOGPFAULTERRORBOX | SEM_NOOPENFILEERRORBOX
    set_mode(get_mode() | suppressed_errors)


class JobBasicLimitInformation(ctypes.Structure):
    """The Win32 `JOBOBJECT_BASIC_LIMIT_INFORMATION` layout describes job limits."""

    _fields_ = [
        ("PerProcessUserTimeLimit", ctypes.c_int64),
        ("PerJobUserTimeLimit", ctypes.c_int64),
        ("LimitFlags", wintypes.DWORD),
        ("MinimumWorkingSetSize", ctypes.c_size_t),
        ("MaximumWorkingSetSize", ctypes.c_size_t),
        ("ActiveProcessLimit", wintypes.DWORD),
        ("Affinity", ctypes.c_size_t),
        ("PriorityClass", wintypes.DWORD),
        ("SchedulingClass", wintypes.DWORD),
    ]


class JobExtendedLimitInformation(ctypes.Structure):
    """The Win32 `JOBOBJECT_EXTENDED_LIMIT_INFORMATION` layout extends basic limits."""

    _fields_ = [
        ("BasicLimitInformation", JobBasicLimitInformation),
        # `IO_COUNTERS` contains six consecutive unsigned 64-bit counters.
        ("IoInfo", ctypes.c_uint64 * 6),
        ("ProcessMemoryLimit", ctypes.c_size_t),
        ("JobMemoryLimit", ctypes.c_size_t),
        ("PeakProcessMemoryUsed", ctypes.c_size_t),
        ("PeakJobMemoryUsed", ctypes.c_size_t),
    ]


class WindowsJob:
    """A Windows job owns the launcher and its ordinary descendant processes.

    The sole job handle belongs to the wrapper. Closing it terminates all job
    processes; no process is permitted to break away from this job.
    """

    def __init__(self):
        """Create a private job whose final handle closure terminates its members."""
        # We declare Win32 signatures because the default `ctypes` integer return
        # type would truncate handles on 64-bit Windows.
        kernel = ctypes.WinDLL("kernel32", use_last_error=True)
        create_job = bind_windows_function(
            kernel, "CreateJobObjectW", wintypes.HANDLE, [ctypes.c_void_p, wintypes.LPCWSTR]
        )
        set_information = bind_windows_function(
            kernel,
            "SetInformationJobObject",
            wintypes.BOOL,
            [wintypes.HANDLE, ctypes.c_int, ctypes.c_void_p, wintypes.DWORD],
        )
        self.assign_process = bind_windows_function(
            kernel, "AssignProcessToJobObject", wintypes.BOOL, [wintypes.HANDLE, wintypes.HANDLE]
        )
        self.open_process = bind_windows_function(
            kernel, "OpenProcess", wintypes.HANDLE, [wintypes.DWORD, wintypes.BOOL, wintypes.DWORD]
        )
        self.close_handle = bind_windows_function(
            kernel, "CloseHandle", wintypes.BOOL, [wintypes.HANDLE]
        )

        # Unnamed jobs with default security attributes have non-inheritable handles.
        self.handle = create_job(None, None)
        if not self.handle:
            raise ctypes.WinError(ctypes.get_last_error())
        try:
            limits = JobExtendedLimitInformation()
            limits.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE
            if not set_information(
                self.handle,
                JOB_OBJECT_EXTENDED_LIMIT_INFORMATION,
                ctypes.byref(limits),
                ctypes.sizeof(limits),
            ):
                raise ctypes.WinError(ctypes.get_last_error())
        except BaseException:
            self.close()
            raise

    def assign(self, process_id):
        """Assign the gated launcher to the job before permitting command creation."""
        # Assignment needs `PROCESS_SET_QUOTA` and `PROCESS_TERMINATE` access.
        process_handle = self.open_process(PROCESS_SET_QUOTA | PROCESS_TERMINATE, False, process_id)
        if not process_handle:
            raise ctypes.WinError(ctypes.get_last_error())
        try:
            if not self.assign_process(self.handle, process_handle):
                raise ctypes.WinError(ctypes.get_last_error())
        finally:
            self.close_handle(process_handle)

    def close(self):
        """Close the sole job handle, terminating any processes still in the job."""
        if self.handle is not None:
            if not self.close_handle(self.handle):
                raise ctypes.WinError(ctypes.get_last_error())
            self.handle = None


if __name__ == "__main__":
    if sys.argv[1:2] == ["--_launcher"]:
        sys.exit(run_launcher(sys.argv[2:]))
    sys.exit(main())
