package metaconfig

import scala.language.experimental.macros

package object generic {
  def deriveSurface[T]: Surface[T] =
    macro metaconfig.internal.Macros.deriveSurfaceImpl[T]
  def deriveDecoder[T](default: T): ConfDecoder[T] =
    macro metaconfig.internal.Macros.deriveConfDecoderImpl[T]
  def deriveDecoderReader[T](default: T): ConfDecoderReader[WithDefault[T], T] =
    macro metaconfig.internal.Macros.deriveConfDecoderReaderImpl[T]
  def deriveEncoder[T]: ConfEncoder[T] =
    macro metaconfig.internal.Macros.deriveConfEncoderImpl[T]
  def deriveCodec[T](default: T): ConfCodec[T] =
    macro metaconfig.internal.Macros.deriveConfCodecImpl[T]
}
