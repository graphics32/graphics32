#!/bin/sh
#
# ~/.bash_aliases example:
# alias lazbuild="~/freepascal/laz/lazbuild"
#
if [ -f ~/.bash_aliases ]; then
  . ~/.bash_aliases
fi
lazbuild Blending/PixelCombine/PixelCombine.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Blending/TextureBlend/TextureBlend.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Drawing/AntiAliasing/AntiAliasing.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Drawing/ArrowHead/ArrowHead.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Drawing/Benchmark/Benchmark.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Drawing/Blurs/Blurs.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Drawing/Clipper/Clipper.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Drawing/CubicSpline/CubicSpline.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Drawing/Curves/Curves.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Drawing/GammaCorrection/GammaCorrection.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Drawing/GradFills/GradFills.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Drawing/GradLines/GradLines.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Drawing/GradSampler/GradSampler.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Drawing/LineStippling/LineStippling.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Drawing/Lion/Lion.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Drawing/MeshGradients/MeshGradients.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Drawing/Polygons/Polygons.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Drawing/RenderText/RenderText.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Drawing/ScatterPlot/ScatterPlot.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Drawing/TextVPR/TextVPR.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Drawing/VertexReduction/LineSimplification.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild General/ByteMaps/ByteMaps.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild General/Image32/Image32.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild General/Mandelbrot/Mandelbrot.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild General/Rotate/Rotate.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Layers/ImgView_Layers/ImgView_Layers.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Layers/RotLayer/RotLayer.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Layers/Sprites/Sprites.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Resampling/NestedSampling/NestedSampling.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Resampling/PixelF/PixelF.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Resampling/Resamplers/Resamplers.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Transformation/ImgWarping/ImgWarping.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Transformation/Transform/Transform.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi
lazbuild Transformation/Visualization/Visualization.lpi
if [ $? -ne 0 ]; then echo 'Press ENTER key to continue...'; read -p '' nothing; fi

