uses
  GR32.CPUID;
...
begin
  // Test for SSE4.1 support
  if (isSSE41 in CPU.InstructionSupport) then
  begin
    ... SSE4.1 specific code here ...
  end;
end;