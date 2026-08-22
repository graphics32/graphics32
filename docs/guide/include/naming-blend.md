Most blend functions are named using a scheme that clearly identify how they operate:

| Name | Example | Meaning |
| --- | --- | --- |
| \*Reg\* | `BlendReg` | The result foreground color is returned as the function `Result`. |
| \*Mem\* | `BlendMem` | The result foreground color is returned in the **F** parameter. |
| Ex\* | `MergeRegEx` | The the foreground alpha is scaled with a master alpha value. |
| \*Mems\* | `BlendMems` | Apply a single foreground color onto an array of background colors. |
| \*Line\* | `MergeLine` | Apply an array of foreground colors onto an array of background colors. |
| \*RGB\* | `BlendMemRGB` | Operate independently on each color channel using separate alpha values. |
| \*Line1\* | `MergeLine1` | Deprecated. Same as \*Mems\*. |
