// shaders.slang

//
// This example builds on the simplistic shaders presented in the
// "Hello, World" example by adding support for (intentionally
// simplistic) surface materil and light shading.
//
// The code here is not meant to exemplify state-of-the-art material
// and lighting techniques, but rather to show how a shader
// library can be developed in a modular fashion without reliance
// on the C preprocessor manual parameter-binding decorations.
//

// We will start with a `struct` for per-view parameters that
// will be allocated into a `ParameterBlock`.
//
// As written, this isn't very different from using an HLSL
// `cbuffer` declaration, but importantly this code will
// continue to work if we add one or more resources (e.g.,
// an enironment map texture) to the `PerView` type.
//
struct PerView
{
    float4x4    viewProjection;
    float3      eyePosition;

    float3      lightDir;
    float3      lightColor;
};
ParameterBlock<PerView>     gViewParams;

// Declaring a block for per-model parameter data is
// similarly simple.
//
struct PerModel
{
    float4x4    modelTransform;
    float4x4    inverseTransposeModelTransform;
};
ParameterBlock<PerModel>    gModelParams;

// We are now going to define a simple model for surface material shading.
//
// The first building block in our model will be the representation of
// the geometry attributes of a surface as fed into the material.
//
struct SurfaceGeometry
{
    float3 position;
    float3 normal;

    // TODO: tangent vectors would be the natural next thing to add here,
    // and would be required for anisotropic materials. However, the
    // simplistic model loading code we are currently using doesn't
    // produce tangents...
    //
    //      float3 tangentU;
    //      float3 tangentV;

    // We store a single UV parameterization in these geometry attributes.
    // A more complex renderer might need support for multiple UV sets,
    // and indeed it might choose to use interfaces and generics to capture
    // the different requirements that different materials impose on
    // the available surface attributes. We won't go to that kind of
    // trouble for such a simple example.
    //
    float2 uv;
};
//
// Next, we want to define the fundamental concept of a refletance
// function, so that we can use it as a building block for other
// parts of the system. This is a case where we are trying to
// show how a proper physically-based renderer (PBR) might
// decompose the problem using Slang, even though our simple
// example is *not* physically based.
//
interface IBRDF
{
    // Technically, a BRDF is only a function of the incident
    // (`wi`) and exitant (`wo`) directions, but for simplicity
    // we are passing in the surface normal (`N`) as well.
    //
    float3 evaluate(float3 wo, float3 wi, float3 N);
};
//
// We can now define various implemntations of the `IBRDF` interface
// that represent different reflectance functions we want to support.
// For now we keep things simple by defining about the simplest
// reflectance function we can think of: the Blinn-Phong reflectance
// model:
//
struct BlinnPhong : IBRDF
{
    // Blinn-Phong needs diffuse and specular reflectances, plus
    // a specular exponent value (which relates to "roughness"
    // in more modern physically-based models).
    //
    float3 kd;
    float3 ks;
    float specularity;

    // Here we implement the one requirement of the `IBRDF` interface
    // for our concrete implementation, using a textbook definition
    // of Blinng-Phong shading.
    //
    // Note: our "BRDF" definition here folds the N-dot-L term into
    // the evlauation of the reflectance function in case there are
    // useful algebraic simplifications this enables.
    //
    float3 evaluate(float3 V, float3 L, float3 N)
    {
        float nDotL = saturate(dot(N, L));
        float3 H = normalize(L + V);
        float nDotH = saturate(dot(N, H));

        return kd*nDotL + ks*pow(nDotH, specularity);
    }
};
//
// It is important to note that a reflectance function is *not*
// a "material." In most cases, a material will have spatially-varying
// properties so that it cannot be summarized as a single `IBRDF`
// instance.
//
// Thus a "material" is a value that can produce a BRDF for any point
// on a surface (e.g., by sampling texture maps, etc.).
//
interface IMaterial
{
    // Different concrete material implementations might yield BRDF
    // values with different types. E.g., one material might yield
    // reflectance functions using `BlinnPhong` while another uses
    // a much more complicated/accurate representation.
    //
    // We encapsulate the choice of BRDF parameters/evaluation in
    // our material interface with an "associated type." In the
    // simplest terms, think of this as an interface requirement
    // that is a type, instead of a method.
    //
    // (If you are C++-minded, you might think of this as akin to
    // how every container provided an `iterator` type, but different
    // containers may have different types of iterators)
    //
    associatedtype BRDF : IBRDF;

    // For our simple example program, it is enough for a material to
    // be able to return a BRDF given a point on the surface.
    //
    // A more complex implementation of material shading might also
    // have the material return updated surface geometry to reflect
    // the result of normal mapping, occlusion mapping, etc. or
    // return an opacity/coverage value for partially transparent
    // surfaces.
    //
    BRDF prepare(SurfaceGeometry geometry);
};

// In order for our shader to be able to take a material
// as a parameter, we need to declare a `ParameterBlock<M>`
// for some material type `M`. Rather than hard-code the
// specific material type to use, or select one via the
// preprocessor, we will use Slang's support for generics,
// by defining a "global type parameter":
//
type_param TMaterial : IMaterial;
//
// This declaration declares a shader parameter `TMaterial`
// that is a to-be-determined *type*. The `TMaterial`
// type parameter is *constrained* to only support types
// that implement our `IMaterial` interface.
//
// With the `TMaterial` parameter declared, we can
// declare that our shader takes as input a parameter block
// containing material data:
//
ParameterBlock<TMaterial>   gMaterial;

// We will now define a trivial first implementation of the material
// interface, which uses our Blinn-Phong BRDF with uniform values
// for its parameters.
//
// Note that this implemetnation is being provided *after* the
// shader parameter `gMaterial` is declared, so that there is no
// assumption in the shader code that `gMaterial` will be plugged
// in using an instance of `SimpleMaterial`
//
//
struct SimpleMaterial : IMaterial
{
    // We declare the properties we need as fields of the material type.
    // When `SimpleMaterial` is used for `TMaterial` above, then
    // `gMaterial` will be a `ParameterBlock<SimpleMaterial>`, and these
    // parameters will be allocated to a constant buffer that is part of
    // that parameter block.
    //
    // TODO: A future version of this example will include texture parameters
    // here to show that they are declared just like simple uniforms.
    //
    float3 diffuseColor;
    float3 specularColor;
    float specularity;

    // To satisfy the requirements of the `IMaterial` interface, our
    // material type needs to provide a suitable `BRDF` type. We
    // do this by using a simple `typedef`, although a nested
    // `struct` type can also satisfy an assocaited type requirement.
    //
    // A future version of the Slang compiler may allow the "right"
    // associated type definition to be inferred from the signature
    // of the `prepare()` method below.
    //
    typedef BlinnPhong BRDF;

    BlinnPhong prepare(SurfaceGeometry geometry)
    {
        BlinnPhong brdf;
        brdf.kd = diffuseColor;
        brdf.ks = specularColor;
        brdf.specularity = specularity;
        return brdf;
    }
};
//
// Note that no other code in this file statically
// references the `SimpleMaterial` type, and instead
// it is up to the application to "plug in" this type,
// or another `IMaterial` implementation for the
// `TMaterial` parameter.
//

// Our vertex shader entry point is only marginally more
// complicated than the Hello World example. We will
// start by declaring the various "connector" `struct`s.
//
struct AssembledVertex
{
    float3 position : POSITION;
    float3 normal   : NORMAL;
    float2 uv       : UV;
};
struct CoarseVertex
{
    float3 worldPosition;
    float3 worldNormal;
    float2 uv;
};
struct VertexStageOutput
{
    CoarseVertex    coarseVertex    : CoarseVertex;
    float4          sv_position     : SV_Position;
};

// Perhaps most interesting new feature of the entry
// point decalrations is that we use a `[shader(...)]`
// attribute (as introduced in HLSL Shader Model 6.x)
// in order to tag our entry points.
//
// This attribute informs the Slang compiler which
// functions are intended to be compiled as shader
// entry points (and what stage they target), so that
// the programmer no longer needs to specify the
// entry point name/stage through the API (or on
// the command line when using `slangc`).
//
// While HLSL added this feature only in newer versions,
// the Slang compiler supports this attribute across
// *all* targets, so that it is okay to use whether you
// want DXBC, DXIL, or SPIR-V output.
//
[shader("vertex")]
VertexStageOutput vertexMain(
    AssembledVertex assembledVertex)
{
    VertexStageOutput output;

    float3 position = assembledVertex.position;
    float3 normal   = assembledVertex.normal;
    float2 uv       = assembledVertex.uv;

    float3 worldPosition = mul(gModelParams.modelTransform, float4(position, 1.0)).xyz;
    float3 worldNormal = mul(gModelParams.inverseTransposeModelTransform, float4(normal, 0.0)).xyz;

    output.coarseVertex.worldPosition = worldPosition;
    output.coarseVertex.worldNormal   = worldNormal;
    output.coarseVertex.uv            = uv;

    output.sv_position = mul(gViewParams.viewProjection, float4(worldPosition, 1.0));

    return output;
}

// Our fragment shader is almost trivial, with the most interesting
// thing being how it uses the `TMaterial` type parameter (through the
// value stored in the `gMaterial` parameter block) to dispatch to
// the correct implementation of the `getDiffuseColor()` method
// in the `IMaterial` interface.
//
// The `gMaterial` parameter block declaration thus serves not only
// to group certain shader parameters for efficient CPU-to-GPU
// communication, but also to select the code that will execute
// in specialized versions of the `fragmentMain` entry point.
//
[shader("fragment")]
float4 fragmentMain(
    CoarseVertex coarseVertex : CoarseVertex) : SV_Target
{
    // We start by using our interpolated vertex attributes
    // to construct the local surface geometry that we will
    // use for material evaluation.
    //
    SurfaceGeometry g;
    g.position = coarseVertex.worldPosition;
    g.normal = normalize(coarseVertex.worldNormal);
    g.uv = coarseVertex.uv;

    float3 V = normalize(gViewParams.eyePosition - g.position);

    // Next we prepare the material, which involves running
    // any "pattern generation" logic of the material (e.g.,
    // sampling and blending texture layers), to produce
    // a BRDF suitable for evaluating under illumination
    // from different light sources.
    //
    // Note that the return type here is `TMaterial.BRDF`,
    // which is the `BRDF` type *assocaited* with the (unknown)
    // `TMaterial` type. When `TMaterial` gets substituted for
    // a concrete type later (e.g., `SimpleMaterial`) this
    // will resolve to a concrete type too (e.g., `SimpleMaterial.BRDF`
    // which is an alias for `BlinnPhong`).
    //
    TMaterial.BRDF brdf = gMaterial.prepare(g);

    // We now begin the light integration step of our surface
    // shader. For this simple example we currently only take
    // the contribution of a single directional light, but
    // this shows the basic idiom for how the evaluation of
    // surface material appearance has been separated into
    // the light independent pattern generation step and the
    // light-dependent BRDF evaluation step.
    //
    float3 color = 0;

    float3 L = normalize(gViewParams.lightDir);
    color += gViewParams.lightColor * brdf.evaluate(V, L, g.normal);

    return float4(color, 1);
}
