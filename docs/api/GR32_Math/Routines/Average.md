---
layout: doc
docType: api
unit: GR32_Math
entity: Average
kind: Function
declaration: "function Average(A, B: Integer): Integer;"
summary: "Computes the integer average of two values without arithmetic overflow."
parameters:
  - name: A
    type: Integer
    description: "First integer."
  - name: B
    type: Integer
    description: "Second integer."
returns:
  - type: Integer
    description: "The average (A + B) div 2."
---

## Description

`Average` computes $(A + B) / 2$ using bitwise identities (`(A and B) + (A xor B) div 2`) to avoid overflow even when $A + B$ exceeds integer range limits.
