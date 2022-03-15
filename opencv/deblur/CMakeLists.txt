cmake_minimum_required(VERSION 3.22.2)

project(deblur)

set (CMAKE_CXX_STANDARD 20)
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

include_directories (
  /usr/local/include 
  /usr/local/opt/opencv/include/opencv4
  ${CMAKE_CURRENT_SOURCE_DIR}
)

link_directories (
  /usr/local/opt/opencv/lib
)

# add_subdirectory(src)

add_executable(deblur main.cpp)

target_link_libraries(deblur 
  opencv_core
  opencv_imgproc
  opencv_highgui
  opencv_gapi
  opencv_stitching
  opencv_alphamat 
  opencv_aruco 
  opencv_barcode
  opencv_bgsegm
  opencv_bioinspired
  opencv_ccalib
  opencv_dnn_objdetect
  opencv_dnn_superres
  opencv_dpm
  opencv_face
  opencv_freetype
  opencv_fuzzy
  opencv_hfs
  opencv_img_hash
  opencv_intensity_transform
  opencv_line_descriptor
  opencv_mcc
  opencv_quality
  opencv_rapid
  opencv_reg
  opencv_rgbd
  opencv_saliency
  opencv_sfm
  opencv_stereo
  opencv_structured_light
  opencv_phase_unwrapping
  opencv_superres
  opencv_optflow
  opencv_surface_matching
  opencv_tracking
  opencv_datasets
  opencv_text
  opencv_plot
  opencv_videostab
  opencv_videoio
  opencv_viz
  opencv_wechat_qrcode
  opencv_xfeatures2d
  opencv_shape
  opencv_ml
  opencv_ximgproc
  opencv_video
  opencv_xobjdetect
  opencv_objdetect
  opencv_calib3d
  opencv_imgcodecs
  opencv_features2d
  opencv_dnn
  opencv_flann
  opencv_xphoto
  opencv_photo
)
