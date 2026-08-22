# Sampling and rasterization

## Sampling

Sampling is a very important concept within digital image processing and image analysis. Sampling is a process where color samples are acquired given their logical coordinates in the (x, y) coordinate space. Graphics32 provides a special class called [TCustomSampler](https://graphics32.github.io/Docs/Units/GR32/Classes/TCustomSampler/_Body.htm), that provides the necessary mechanism for implementing different sampling techniques. A _sampler_ can be conceived as a scalar function f(x, y) that returns a color sample given a logical coordinate (x, y). A sample may be created synthetically (this is a common technique within ray-tracing, fractal rendering and pattern generation). It may also be acquired from some input hardware device. Another very common method for acquiring samples is _resampling_.

## Resampling

[Resampling](https://en.wikipedia.org/wiki/Image_scaling) is the process of reconstructing samples from a discrete input signal. The idea can also be extended from the 1D case to 2D. In the 2D case we can think of the bitmap as our signal. We have a number of pixels, aligned on a rectangular square grid. Hence we only know the actual color values at a number of discrete coordinates. In order to determine the color value of a sample at an arbitrary coordinate in a continuous image space, we need to perform interpolation for reconstructing this sample.

Descendants of [TCustomResampler](https://graphics32.github.io/Docs/Units/GR32/Classes/TCustomResampler/_Body.htm) implement various algorithms for performing resampling and sample acquisition. A general algorithm reconstructing samples is to perform convolution in a local neighborhood of the actual sample coordinate. This method is used in [TKernelResampler](https://graphics32.github.io/Docs/Units/GR32_Resamplers/Classes/TKernelResampler/_Body.htm), where a convolution filter is specified by the [TKernelSampler.Kernel](https://graphics32.github.io/Docs/Units/GR32_Resamplers/Classes/TKernelSampler/Properties/Kernel.htm) property.

Graphics32 includes a class called [TCustomKernel](https://graphics32.github.io/Docs/Units/GR32_Resamplers/Classes/TCustomKernel/_Body.htm) which is used as an ancestor class for various convolution kernels. For high quality resampling, one should consider using a kernel that approximates the ideal low-pass filter. The ideal low-pass filter is often referred to as a _sinc_ filter. It can be described by the formula

sinc(x)=sin(πx)πxsinc(x)=sin(πx)πx

Since this function has infinite extent, it is not practical for using as a convolution kernel (because of the computational overhead). [TWindowedSincKernel](https://graphics32.github.io/Docs/Units/GR32_Resamplers/Classes/TWindowedSincKernel/_Body.htm) is a base class for kernels that use the _sinc_ function together with a _[window function](https://en.wikipedia.org/wiki/Window_function)_ (also known as _[tapering function](https://en.wikipedia.org/wiki/Tapering_\(mathematics\))_ or _[apodization function](https://en.wikipedia.org/wiki/Apodization)_). This way the kernel can be constrained to a certain width and reduce the amount of computations.

For further details about resampling, see [Resamplers_Ex](https://graphics32.github.io/Docs/Examples.htm#Resamplers%20Example) example project.

## Rasterization

By _rasterizing_ an image, we collect samples for each pixel of an output bitmap. The _rasterizer_ is responsible for the order in which output pixels are sampled and how the destination bitmap is updated. A rasterizer class is derived from [TRasterizer](https://graphics32.github.io/Docs/Units/GR32_Rasterizers/Classes/TRasterizer/_Body.htm), by overriding the protected _DoRasterize_ method.

Instances of TRasterizer need to be associated with a sampler and an output destination bitmap. Some rasterization schemes, such as _swizzling_ , may improve cache-performance for certain applications, since samples are collected in a local neighborhood rather than row by row. Rasterizers can also provide various transition effects for creating transitions between bitmaps.

Graphics32 includes the following rasterizers:

[TRegularRasterizer](https://graphics32.github.io/Docs/Units/GR32_Rasterizers/Classes/TRegularRasterizer/_Body.htm)| Rasterizes the bitmap row by row.
---|---
[TProgressiveRasterizer](https://graphics32.github.io/Docs/Units/GR32_Rasterizers/Classes/TProgressiveRasterizer/_Body.htm)| Rasterizes in a progressive manner by successively increasing the resolution of the image.
[TTesseralRasterizer](https://graphics32.github.io/Docs/Units/GR32_Rasterizers/Classes/TTesseralRasterizer/_Body.htm)| Rasterization by sub-division.
[TContourRasterizer](https://graphics32.github.io/Docs/Units/GR32_Rasterizers/Classes/TContourRasterizer/_Body.htm)| The rasterization path is determined from the intensity of the collected samples.

## Nested sampling

If the input of one sampler is the output from another, then we have a _nested sampler_. Nested samplers are derived from the class [TNestedSampler](https://graphics32.github.io/Docs/Units/GR32_Resamplers/Classes/TNestedSampler/_Body.htm).

By nesting samplers, it is possible to create a chain of nested samplers between the sampler that generates the actual sample and the rasterizer. This mechanism is illustrated in the below image.

![](/images/img_nested.gif)

There are many different useful applications for nested samplers. A sampler may be associated with a transformation. This will transform the input coordinate that is passed to the sampler at the next level.

It is possible to collect more than one sample in a local neighborhood of the pixel coordinate of the output pixel. This permits the use of techniques such as _super sampling_ , where several samples are collected in order to estimate the color of the area covered by a pixel in the destination bitmap. If super sampling is not performed, it may cause jagginess and aliasing artifacts in the output image. However, this also depends on what kind of reconstruction method is used if samples are resampled.

Another important class of nested samplers is _kernel samplers_. Kernel samplers compute an output sample from several subsamples in a local region of the input coordinate. Each subsample is combined with a kernel value (contained within a [TIntegerMap](https://graphics32.github.io/Docs/Units/GR32_OrdinalMaps/Classes/TIntegerMap/_Body.htm) object). A class-specific kernel operation is used to update a buffer for each collected sample. This permits a very simplistic implementation of convolution and morphological operations.

The following is a list of the different nested samplers that are included in Graphics32.

  * Transformers
    * [TTransformer](https://graphics32.github.io/Docs/Units/GR32_Resamplers/Classes/TTransformer/_Body.htm) — transforms coordinates using an associated [TTransformation](https://graphics32.github.io/Docs/Units/GR32_Transforms/Classes/TTransformation/_Body.htm) object;
    * [TNearestTransformer](https://graphics32.github.io/Docs/Units/GR32_Resamplers/Classes/TNearestTransformer/_Body.htm) — the same as above, but for nearest neighbor resampling.
  * Super samplers
    * [TSuperSampler](https://graphics32.github.io/Docs/Units/GR32_Resamplers/Classes/TSuperSampler/_Body.htm) — performs regular super sampling;
    * [TAdaptiveSuperSampler](https://graphics32.github.io/Docs/Units/GR32_Resamplers/Classes/TAdaptiveSuperSampler/_Body.htm) — performs adaptive super sampling;
    * [TPatternSampler](https://graphics32.github.io/Docs/Units/GR32_Resamplers/Classes/TPatternSampler/_Body.htm) — performs sampling according to a predefined pattern.
  * Kernel samplers
    * [TConvolver](https://graphics32.github.io/Docs/Units/GR32_Resamplers/Classes/TConvolver/_Body.htm) — performs convolution;
    * [TSelectiveConvolver](https://graphics32.github.io/Docs/Units/GR32_Resamplers/Classes/TSelectiveConvolver/_Body.htm) — performs selective convolution;
    * [TDilater](https://graphics32.github.io/Docs/Units/GR32_Resamplers/Classes/TDilater/_Body.htm) — performs morphological dilation;
    * [TEroder](https://graphics32.github.io/Docs/Units/GR32_Resamplers/Classes/TEroder/_Body.htm) — performs morphological erosion;
    * [TExpander](https://graphics32.github.io/Docs/Units/GR32_Resamplers/Classes/TExpander/_Body.htm) — special expansion operation;
    * [TContracter](https://graphics32.github.io/Docs/Units/GR32_Resamplers/Classes/TContracter/_Body.htm) — special contraction operation.



For further details about nested sampling, see the [NestedSampling_Ex](https://graphics32.github.io/Docs/Examples.htm#Nested%20Sampling%20Example) example project.
