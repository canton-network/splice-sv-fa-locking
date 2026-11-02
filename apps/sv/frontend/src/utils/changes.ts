// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

export function wrapMicroseconds(microseconds: string | null): { microseconds: string } | null {
  return microseconds !== null && microseconds !== '' ? { microseconds } : null;
}

export function nullIfAllNull<A extends Record<string, unknown>>(a: A): A | null {
  return Object.values(a).some(v => v !== null) ? a : null;
}
