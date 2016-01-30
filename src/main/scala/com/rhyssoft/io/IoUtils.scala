package com.rhyssoft.io

import java.io.Closeable

object IoUtils {
  def autoClose[A <: Closeable,B](c: A)(f: A => B) = try f(c) finally c.close()
}
