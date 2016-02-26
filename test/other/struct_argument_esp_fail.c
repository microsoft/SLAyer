/* Copyright (c) Microsoft Corporation.  All rights reserved. */

typedef struct _PA {
  int i;
} PHYSICAL_ADDRESS;

typedef struct {
  struct {
    PHYSICAL_ADDRESS f;
  } t;
} S, *PS;

func(PHYSICAL_ADDRESS p) {
  p.i = 10;
}

func2(S s) {
  s.t.f.i = 10;
}

main() {
  PS d;
  d = (PS)malloc(sizeof(S));
  func(d->t.f);
  func2(*d);
}
