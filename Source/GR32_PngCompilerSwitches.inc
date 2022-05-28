{ ------------------------------------------------------------------------- }
{ Various compiler switches to control the behaviour of th GR32_PNG library }
{ ------------------------------------------------------------------------- }

{ Enable to check the CRC of each chunk in a PNG file and the CRC of the
  container itself. In case of a mismatch an exception is raised.
  The default is enabled. }
{$DEFINE CheckCRC}

{ Enable to validate every read operation. Usually the necessary length of the
  input stream for a chunk is checked before reading the chunk, but some
  checks might still be missing. By enabling this option every data request
  that can not be fulfilled will raise an exception. This check leads to an
  unnecessary higher CPU usage, so enable only when an error in the library is
  suspected.
  The default is disabled. }
{-$DEFINE ValidateEveryReadOperation}

{ The company base2 offers a library called ZLibEx, which uses optimized object
  files compiled with the latest and fastest C compilers. The performance boosts
  especially for the latest IDEs might be small, but it is worth to try this
  library. It can be downloaded from: base2ti.com
  The default is disabled. }
{-$DEFINE ZLibEx}

{ When writing PNG files, it is possible to store a gamma correction along with
  the PNG file. Though no user definalbe handling for gamma correction has been
  implemented, it is possible to store a simple 1:1 correction table in the PNG
  file. This will increase the file, but might improve compatibility.
  The default is disabled. }
{-$DEFINE StoreGamma}

{ The library makes use of x86 assembler optimizations. While this results in a
  (small) performance boost it makes the library platform dependent.
  By enabling the PUREPASCAL switch, only pascal code is used, avoiding any
  assmbler code. The code is likely slower, but probably more platform
  independent.
  The default is disabled. }
{-$DEFINE PUREPASCAL}
