###############################################################################
#
# Execute this file to replace all example .dproj-files with files 
# based on the template.dproj file.
#
###############################################################################
#
# The script transforms the template for each project in the following
# way:
#
#   1) Replaces the project GUID to ensure that the files are unique.
#
#   2) Updates the name of the project and the .dpr-file.
#
#   3) Updates unit references.
#      For each .pas-file in the project folder a reference will be added.
#
#   4) Updates resource script reference.
#      For each .rc-file in the project folder a reference will be added.
#
# It is assumed that the example project has the following properties:
#
#   1) The project is located two levels down from the Examples folder.
#      For example: Examples\Foo\Bar\MyProject.dpr
#
###############################################################################

#
# Get content of template file (-raw because we're doing multi line matches)
#
$template = Get-Content -path .\template.dproj -Raw


#
# Get list of project files in child folders
#
$Files = Get-ChildItem -Recurse -Filter '*.dproj' -Exclude template.dproj


#
# Iterate project file list
#
foreach ($File in $Files) {

  "Processing $($File.BaseName)..."

  #
  # Adjust template to match project name
  #
  $Output = $template `
    -Replace '(\<ProjectGuid\>)\{.*\}(\</ProjectGuid\>)', "`$1{$(New-Guid)}`$2" `
    -Replace '(\<MainSource\>).*(\.dpr\</MainSource\>)', "`$1$($File.BaseName)`$2" `
    -Replace '(\<SanitizedProjectName\>).*(\</SanitizedProjectName\>)', "`$1$($File.BaseName)`$2" `
    -Replace '(\<Source Name=\"MainSource\"\>).*(\.dpr\</Source\>)', "`$1$($File.BaseName)`$2"


  #
  # Construct list of references to .pas- and .dfm-files
  #
  $References = ''
  $Units = Get-ChildItem -Path $File.Directory  -Filter '*.pas'
  foreach ($Unit in $Units) {
    $References = $References + "<DCCReference Include=`"$($Unit.Name)`"/>`n"
  }
  # Replace unit reference placeholder with new list of references
  $Output = $Output `
    -Replace '\<DCCReference.*/\>', $References


  #
  # Construct list of references to .rc-files
  #
  $References = ''
  $Units = Get-ChildItem -Path $File.Directory  -Filter '*.rc'
  foreach ($Unit in $Units) {
    $References = $References + "<RcCompile Include=`"$($Unit.Name)`"><Form>$($Unit.BaseName).res</Form></RcCompile>`n"
  }
  # Replace resource file reference placeholder with new list of references
  $Output = $Output `
    -Replace '(?si)\<RcCompile.*\</RcCompile\>', $References


  #
  # Replace project file
  #
  $Output | Out-File -Encoding UTF8 -FilePath $File.FullName

}
