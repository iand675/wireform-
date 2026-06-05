-- | A focused port of the @lucasb-eyer/go-colorful@ math needed by the
-- terminal color model: sRGB -> linear RGB -> CIEXYZ -> CIELUV -> LCh -> HSLuv,
-- and the Euclidean distance in HSLuv space.
--
-- @termenv@ uses 'distanceHSLuv' to choose the nearest 256- or 16-color
-- approximation of a truecolor value, so a faithful port of the color
-- degradation requires this perceptual distance.
module TTY.Color.HSLuv
  ( RGB (..)
  , hsluv
  , distanceHSLuv
  ) where


-- | An sRGB color with each channel in @[0, 1]@.
data RGB = RGB
  { rgbR :: !Double
  , rgbG :: !Double
  , rgbB :: !Double
  }
  deriving stock (Eq, Show)


-- | HSLuv white reference (a rounded D65); see go-colorful's note.
hSLuvD65 :: (Double, Double, Double)
hSLuvD65 = (0.95045592705167, 1.0, 1.089057750759878)


kappa :: Double
kappa = 903.2962962962963


epsilon :: Double
epsilon = 0.0088564516790356308


-- | Row-major sRGB(D65) -> XYZ inverse matrix used by go-colorful's bounds.
mMatrix :: [[Double]]
mMatrix =
  [ [3.2409699419045214, -1.5373831775700935, -0.49861076029300328]
  , [-0.96924363628087983, 1.8759675015077207, 0.041555057407175613]
  , [0.055630079696993609, -0.20397695888897657, 1.0569715142428786]
  ]


clamp01 :: Double -> Double
clamp01 = max 0 . min 1


sq :: Double -> Double
sq v = v * v


linearize :: Double -> Double
linearize v
  | v <= 0.04045 = v / 12.92
  | otherwise = ((v + 0.055) / 1.055) ** 2.4


linearRgb :: RGB -> (Double, Double, Double)
linearRgb (RGB r g b) = (linearize r, linearize g, linearize b)


linearRgbToXyz :: (Double, Double, Double) -> (Double, Double, Double)
linearRgbToXyz (r, g, b) =
  ( 0.41239079926595948 * r + 0.35758433938387796 * g + 0.18048078840183429 * b
  , 0.21263900587151036 * r + 0.71516867876775593 * g + 0.072192315360733715 * b
  , 0.019330818715591851 * r + 0.11919477979462599 * g + 0.95053215224966058 * b
  )


xyzToUv :: Double -> Double -> Double -> (Double, Double)
xyzToUv x y z =
  let denom = x + 15.0 * y + 3.0 * z
   in if denom == 0.0
        then (0.0, 0.0)
        else (4.0 * x / denom, 9.0 * y / denom)


xyzToLuvWhiteRef :: (Double, Double, Double) -> (Double, Double, Double)
xyzToLuvWhiteRef (x, y, z) =
  let (w0, w1, w2) = hSLuvD65
      l =
        if y / w1 <= (6.0 / 29.0) ** 3.0
          then y / w1 * kappa / 100.0
          else 1.16 * (y / w1) ** (1.0 / 3.0) - 0.16
      (ubis, vbis) = xyzToUv x y z
      (un, vn) = xyzToUv w0 w1 w2
      u = 13.0 * l * (ubis - un)
      v = 13.0 * l * (vbis - vn)
   in (l, u, v)


-- | CIELUV -> cylindrical LCh; @h@ in degrees @[0, 360)@.
luvToLuvLCh :: (Double, Double, Double) -> (Double, Double, Double)
luvToLuvLCh (bigL, u, v) =
  let h =
        if abs (v - u) > 1e-4 && abs u > 1e-4
          then properMod (57.29577951308232087721 * atan2 v u + 360.0) 360.0
          else 0.0
      c = sqrt (sq u + sq v)
   in (bigL, c, h)


-- | Go's @math.Mod@ truncates toward zero; with the @+360@ bias the operands
-- here are non-negative, so a plain remainder matches.
properMod :: Double -> Double -> Double
properMod a b = a - b * fromIntegral (truncate (a / b) :: Integer)


getBounds :: Double -> [(Double, Double)]
getBounds l =
  let sub1 = (l + 16.0) ** 3.0 / 1560896.0
      sub2 = if sub1 > epsilon then sub1 else l / kappa
      forRow row =
        let m0 = row !! 0
            m1 = row !! 1
            m2 = row !! 2
            forK k =
              let top1 = (284517.0 * m0 - 94839.0 * m2) * sub2
                  top2 = (838422.0 * m2 + 769860.0 * m1 + 731718.0 * m0) * l * sub2 - 769860.0 * fromIntegral k * l
                  bottom = (632260.0 * m2 - 126452.0 * m1) * sub2 + 126452.0 * fromIntegral k
               in (top1 / bottom, top2 / bottom)
         in [forK (0 :: Int), forK 1]
   in concatMap forRow mMatrix


lengthOfRayUntilIntersect :: Double -> Double -> Double -> Double
lengthOfRayUntilIntersect theta x y = y / (sin theta - x * cos theta)


maxChromaForLH :: Double -> Double -> Double
maxChromaForLH l h =
  let hRad = h / 360.0 * pi * 2.0
      lengths = do
        (x, y) <- getBounds l
        let len = lengthOfRayUntilIntersect hRad x y
        [len | len > 0.0]
   in if null lengths then 1 / 0 else minimum lengths


luvLChToHSLuv :: (Double, Double, Double) -> (Double, Double, Double)
luvLChToHSLuv (l0, c0, h) =
  let c = c0 * 100.0
      l = l0 * 100.0
      s =
        if l > 99.9999999 || l < 0.00000001
          then 0.0
          else let mx = maxChromaForLH l h in c / mx * 100.0
   in (h, clamp01 (s / 100.0), clamp01 (l / 100.0))


-- | The HSLuv (hue, saturation, lightness) of an sRGB color. Hue is in
-- @[0, 360]@, saturation and lightness in @[0, 1]@.
hsluv :: RGB -> (Double, Double, Double)
hsluv = luvLChToHSLuv . luvToLuvLCh . xyzToLuvWhiteRef . linearRgbToXyz . linearRgb


-- | Euclidean distance in HSLuv space, with hue scaled by 1/100 so the three
-- axes share a comparable range (matches go-colorful's @DistanceHSLuv@).
distanceHSLuv :: RGB -> RGB -> Double
distanceHSLuv c1 c2 =
  let (h1, s1, l1) = hsluv c1
      (h2, s2, l2) = hsluv c2
   in sqrt (sq ((h1 - h2) / 100.0) + sq (s1 - s2) + sq (l1 - l2))
