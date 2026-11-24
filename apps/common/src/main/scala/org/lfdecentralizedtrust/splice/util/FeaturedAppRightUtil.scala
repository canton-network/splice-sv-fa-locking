// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.util

import org.lfdecentralizedtrust.splice.codegen.java.splice.amulet.FeaturedAppRight

import scala.jdk.OptionConverters.*

object FeaturedAppRightUtil {
  def providerIsObserver(right: FeaturedAppRight): Boolean =
    right.providerIsObserver.toScala != Some(false)
}
