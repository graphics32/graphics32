---
layout: doc
docType: api
unit: GR32_Resamplers
entity: RegisterKernel
kind: Function
declaration: "procedure RegisterKernel(KernelClass: TCustomKernelClass);"
summary: "Registers reconstruction kernel classes into the global kernel class registry."
parameters:
  - name: KernelClass
    type: TCustomKernelClass
    description: "Class reference of the spatial reconstruction kernel to register."
---

## Description

`RegisterKernel` adds a kernel class to the global [[KernelList]] registry, allowing kernel resamplers to instantiate kernels dynamically by class reference or string name.
