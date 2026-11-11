---
layout: doc
docType: api
unit: GR32_VectorMaps
parent: TVectorMap
entity: TVectorMap.SaveToFile
kind: Method
declaration: "procedure SaveToFile(const FileName: string);"
summary: "Saves displacement vectors to an Adobe Photoshop Liquify mesh file."
parameters:
  - name: FileName
    type: string
    description: "Path to destination Photoshop Liquify mesh file."
---

## Description

`SaveToFile` exports the vector map buffer into the Adobe Photoshop Liquify mesh file format (`.msh`).
