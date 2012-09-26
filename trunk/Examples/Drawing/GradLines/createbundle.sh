#!/bin/sh
# Force Bourne shell in case tcsh is default.
#
appname=GradLines
appfolder=$appname.app
macosfolder=$appfolder/Contents/MacOS
plistfile=$appfolder/Contents/Info.plist
appfile=$appname
#
if ! [ -e $appfile ]
then
  echo "$appfile does not exist"
elif [ -e $appfolder ]
then
  echo "$appfolder already exists"
else
  echo "Creating $appfolder..."
  mkdir $appfolder
  mkdir $appfolder/Contents
  mkdir $appfolder/Contents/MacOS
  mkdir $appfolder/Contents/Resources
#
# Instead of copying executable into .app folder after each compile,
# simply create a symbolic link to executable.
  ln -s ../../../$appname $macosfolder/$appname
# Also create a symbolic link for the resource files
  ln -s ../../../../../../Media $appfolder/Contents/Resources/Media
#
# Create PkgInfo file.
  echo "APPL????" >$appfolder/Contents/PkgInfo
#
# Create information property list file (Info.plist).
  echo '<?xml version="1.0" encoding="UTF-8"?>' >$plistfile
  echo '<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">' >>$plistfile
  echo '<plist version="1.0">' >>$plistfile
  echo '<dict>' >>$plistfile
  echo '  <key>CFBundleDevelopmentRegion</key>' >>$plistfile
  echo '  <string>English</string>' >>$plistfile
  echo '  <key>CFBundleExecutable</key>' >>$plistfile
  echo '  <string>'$appname'</string>' >>$plistfile
  echo '  <key>CFBundleInfoDictionaryVersion</key>' >>$plistfile
  echo '  <string>6.0</string>' >>$plistfile
  echo '  <key>CFBundlePackageType</key>' >>$plistfile
  echo '  <string>APPL</string>' >>$plistfile
  echo '  <key>CFBundleSignature</key>' >>$plistfile
  echo '  <string>????</string>' >>$plistfile
  echo '  <key>CFBundleVersion</key>' >>$plistfile
  echo '  <string>1.0</string>' >>$plistfile
  echo '  <key>CSResourcesFileMapped</key>' >>$plistfile
  echo '  <true/>' >>$plistfile
  echo '</dict>' >>$plistfile
  echo '</plist>' >>$plistfile
fi