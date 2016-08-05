module Yentl.Model.PoincareDisc (
  PoincareDisc(), unPD, lineType, LineType(..), idealPoints, mkPoincareDisc, originPoincareDisc, interpolatePD
) where

import qualified Yentl.Algebra as A
import Yentl.Geo
import Yentl.Geometry
import Yentl.View.Cartesian

{----------------}
{- PoincareDisc -}
{----------------}

data PoincareDisc a = PD
  { unPD :: (a,a)
  } deriving (Eq, Show)

originPoincareDisc :: (Num a) => PoincareDisc a
originPoincareDisc = PD (0,0)

mkPoincareDisc :: (Ord a, Num a) => (a,a) -> Maybe (PoincareDisc a)
mkPoincareDisc (x,y) = if x^2 + y^2 >= 1
  then Nothing
  else Just $ PD (x,y)

instance (Fractional a) => Coords (PoincareDisc a) where
  coords (x,y) = if x^2 + y^2 >= 1
    then report $ BadCoordinates (fromRational x, fromRational y)
    else return $ PD { unPD = (fromRational x, fromRational y) }


data LineType a
  = TypeI  (A.Slope a) -- slope of line through origin
  | TypeII (a,a)       -- coordinates of ideal center
  deriving Show


lineType :: (Ord a, Num a, Fractional a, A.SquareRoot a)
  => (a,a) -> (a,a) -> Maybe (LineType a)
lineType a@(ax,ay) b@(bx,by) = do
  if a == b
    then Nothing
    else if ax*by == ay*bx
      then
        let
          Just m = A.slope (bx - ax) (by - ay)
        in Just $ TypeI m
      else
        let
          unit = A.ceCenterRadius (0,0) 1
          Just c = A.ceInvertPoint unit a
          Just circ = A.cePointPointPoint a b c
          p = A.ceCenterOf circ
        in Just $ TypeII p

idealPoints :: (Ord a, Num a, Fractional a, A.SquareRoot a)
  => (a,a) -> (a,a) -> Maybe ((a,a),(a,a))
idealPoints a@(ax,ay) b@(bx,by) = do
  let True = ax^2 + ay^2 < 1
  let True = bx^2 + by^2 < 1
  t <- lineType a b
  let unit = A.ceCenterRadius (a A.-. a) 1
  case t of
    TypeI m -> do
      let
        Just ell = A.lfPointSlope (0,0) m
        A.Only2of2 u v = A.solve2var2sol (unit, ell)
        seg = A.seEndpoints u b
      if A.isSolutionOf2 a seg
        then Just (u,v)
        else Just (v,u)

    TypeII o -> do
      let
        circ = A.ceCenterPoint o a
        A.Only2of2 u v = A.solve2var2sol (unit, circ)
        Just arc = A.maCenterEndpoints o (u,b)
      if A.isSolutionOf2 a arc
        then Just (u,v)
        else Just (v,u)

interpolatePD :: (Ord a, Num a, Fractional a, A.SquareRoot a)
  => a -> PoincareDisc a -> PoincareDisc a -> PoincareDisc a
interpolatePD t (PD a) (PD b) = case lineType a b of
  Nothing -> PD a

  Just (TypeI m) ->
    let Just (u,v) = idealPoints a b in
    let seg = A.seEndpoints u v in
    PD $ A.along t seg

  Just (TypeII o) ->
    let Just (u,v) = idealPoints a b in
    let Just arc = A.maCenterEndpoints o (u,v) in
    PD $ A.along t arc

idealCenter :: (Ord a, Num a, Fractional a, A.SquareRoot a)
  => (a,a) -> (a,a) -> Maybe (a,a)
idealCenter o@(ox,oy) a@(ax,ay) = do
  t <- lineType o a
  if o == (0,0)
    then Just o
    else case t of
      TypeI _ -> do
        let unit = A.ceCenterRadius (0,0) 1
        let Just o' = A.ceInvertPoint unit o
        let circ = A.ceCenterPoint (A.midpoint o' o) o
        let Just a' = A.ceInvertPoint circ a
        Just $ A.midpoint a a'

      TypeII w -> do
        let Just ell1 = A.lfPointFoot w a
        let Just ell2 = A.lfPointPoint (0,0) o
        let Just p = A.solve2var1sol (ell1, ell2)
        Just p

circlePD o a =
  if o == a
    then Nothing
    else do
      let Just p = idealCenter o a
      Just $ A.ceCenterPoint p a



{----------------------}
{- Incidence Geometry -}
{----------------------}

instance (Ord a, Num a, Fractional a, A.SquareRoot a)
  => IncidenceGeometry (PoincareDisc a) where
  areCollinear (PD a) (PD b) (PD c) = case lineType a b of
    Nothing -> a /= c

    Just (TypeI m) ->
      let Just ell = A.lfPointSlope (0,0) m in
      A.isSolutionOf2 c ell

    Just (TypeII o) ->
      let circ = A.ceCenterPoint o a in
      A.isSolutionOf2 c circ


  intersectLines (PD a, PD b) (PD u, PD v) = do
    tab <- lineType a b
    tuv <- lineType u v
    case (tab, tuv) of
      (TypeI mab, TypeI muv) -> do
        if mab == muv
          then Nothing
          else Just $ PD (0,0)

      (TypeI mab, TypeII ouv) -> do
        let
          Just ell = A.lfPointSlope (0,0) mab
          Just wuv = idealPoints u v
          Just arc = A.maCenterEndpoints ouv wuv
        case A.solve2var2sol (ell, arc) of
          A.Only1of2 (zx,zy) -> do
            Just $ PD (zx,zy)
          _ -> Nothing

      (TypeII oab, TypeI muv) -> do
        let
          Just ell = A.lfPointSlope (0,0) muv
          Just wab = idealPoints a b
          Just arc = A.maCenterEndpoints oab wab
        case A.solve2var2sol (ell, arc) of
          A.Only1of2 (zx,zy) -> do
            Just $ PD (zx,zy)
          _ -> Nothing

      (TypeII oab, TypeII ouv) -> do
        let
          Just wab = idealPoints a b
          Just arc1 = A.maCenterEndpoints oab wab
          Just wuv = idealPoints u v
          Just arc2 = A.maCenterEndpoints ouv wuv
        case A.solve2var2sol (arc1, arc2) of
          A.Only1of2 (zx,zy) -> do
            Just $ PD (zx,zy)
          _ -> Nothing




instance (Ord a, Num a, Fractional a, A.SquareRoot a)
  => OrderedGeometry (PoincareDisc a) where
  isBetween (PD x) (PD a, PD b) = case lineType a b of
    Nothing -> False

    Just (TypeI _) ->
      let seg = A.seEndpoints a b in
      (A.isSolutionOf2 x seg) && (x /= a) && (x /= b)

    Just (TypeII o) ->
      let Just arc = A.maCenterEndpoints o (a,b) in
      (A.isSolutionOf2 x arc) && (x /= a) && (x /= b)


  interpolate (PD a) (PD b) = do
    t <- lineType a b
    (u,v) <- idealPoints a b
    case t of
      TypeI m -> do
        let Just x = mkPoincareDisc $ A.midpoint u a
        let Just y = mkPoincareDisc $ A.midpoint a b
        let Just z = mkPoincareDisc $ A.midpoint b v
        return (x,y,z)

      TypeII o -> do
        let Just sua = A.maCenterEndpoints o (u,a)
        let Just sab = A.maCenterEndpoints o (a,b)
        let Just sbv = A.maCenterEndpoints o (b,v)
        let Just x = mkPoincareDisc $ A.maMidpoint sua 
        let Just y = mkPoincareDisc $ A.maMidpoint sab
        let Just z = mkPoincareDisc $ A.maMidpoint sbv
        return (x,y,z)


  onSameSideOf (PD u@(ux,uy)) (PD v@(vx,vy)) (PD a@(ax,ay), PD b) =
    case lineType a b of
      Nothing -> False

      Just (TypeI m) ->
        let
          du = A.orientation a b u
          dv = A.orientation a b v
        in
          if du == A.Zero || dv == A.Zero
            then False
            else du == dv

      Just (TypeII (ox,oy)) ->
        let
          da = (ax-ox)^2 + (ay-oy)^2
          du = (ux-ox)^2 + (uy-oy)^2
          dv = (vx-ox)^2 + (vy-oy)^2
        in
          case (compare du da, compare dv da) of
            (LT,LT) -> True
            (GT,GT) -> True
            _       -> False



instance (Ord a, Num a, Fractional a, A.SquareRoot a)
  => CongruenceGeometry (PoincareDisc a) where
  segmentCongruence (PD a1, PD b1) (PD a2, PD b2) =
    let
      Just (p1,q1) = idealPoints a1 b1
      Just (p2,q2) = idealPoints a2 b2
      d1 = ((A.dotSq $ q1 A.-. a1)*(A.dotSq $ p1 A.-. b1)) /
           ((A.dotSq $ p1 A.-. a1)*(A.dotSq $ q1 A.-. b1))
      d2 = ((A.dotSq $ q2 A.-. a2)*(A.dotSq $ p2 A.-. b2)) /
           ((A.dotSq $ p2 A.-. a2)*(A.dotSq $ q2 A.-. b2))
    in d1 == d2


  angleCongruence (PD a1, PD o1, PD b1) (PD a2, PD o2, PD b2) =
    let
      oa1 = idealPoints o1 a1
      ob1 = idealPoints o1 b1
      oa2 = idealPoints o2 a2
      ob2 = idealPoints o2 b2
    in
      case (oa1, ob1, oa2, ob2) of
        (Just (u1,v1), Just (s1,t1), Just (u2,v2), Just (s2,t2)) ->
          let
            p1 = (A.dot (u1 A.-. v1) (s1 A.-. t1))
                   + ((A.dot u1 t1)*(A.dot v1 s1))
                   - ((A.dot u1 s1)*(A.dot v1 t1))
            q1 = (1 - (A.dot u1 v1))^2
            r1 = (1 - (A.dot s1 t1))^2
            w1 = p1^2 / (q1 * r1)
            p2 = (A.dot (u2 A.-. v2) (s2 A.-. t2))
                   + ((A.dot u2 t2)*(A.dot v2 s2))
                   - ((A.dot u2 s2)*(A.dot v2 t2))
            q2 = (1 - (A.dot u2 v2))^2
            r2 = (1 - (A.dot s2 t2))^2
            w2 = p2^2 / (q2 * r2)
          in w1 == w2

        _ -> False


instance (Ord a, Num a, Fractional a, A.SquareRoot a)
  => PlaneGeometry (PoincareDisc a) where
  cutCircleRay (PD o, PD a) (PD b) = do
    p <- idealCenter o a
    let circ = A.ceCenterPoint p a
    t <- lineType o b
    case t of
      TypeI m -> do
        let Just (_,v) = idealPoints o b
        let seg = A.seEndpoints o v
        let A.Only1of2 h = A.solve2var2sol (circ, seg)
        Just (PD h)

      TypeII w -> do
        let Just (_,v) = idealPoints o b
        let Just arc = A.maCenterEndpoints w (o,v)
        let A.Only1of2 h = A.solve2var2sol (circ, arc)
        Just (PD h)

  cutCircles (a'@(PD a), o'@(PD o), b'@(PD b)) (x'@(PD x), p'@(PD p), y'@(PD y)) =
    if not (isBetween x' (a',b') && isBetween b' (x',y'))
      then Nothing
      else do
        let Just circ1 = circlePD o a
        let Just circ2 = circlePD p x
        let A.Only2of2 h k = A.solve2var2sol (circ1, circ2)
        Just (PD h, PD k)







instance (Ord a, Num a, Fractional a, A.SquareRoot a, A.ApproximateRational a)
  => ToCartesian (PoincareDisc a) where
  playfield = Playfield [CartesianCircle dashed (0,0) 1]

  toCartesian command =
    let
      smear a = A.approximateWithin (1/1000) a
      fudge (x,y) = (smear x, smear y)
    in
      case command of
        DrawPoint s (PD x) ->
          [CartesianPoint s (fudge x)]


        DrawLine s (PD x) (PD y) ->
          let Just t = lineType x y in
          case t of
            TypeI m ->
              let Just (u,v) = idealPoints x y in
              [CartesianSegment s (fudge u) (fudge v)]

            TypeII w ->
              let Just (u,v) = idealPoints x y in
              let Just r = A.root2 $ A.dotSq (u A.-. w) in
              let tu = A.arctan2 $ fudge (u A.-. w) in
              let tv = A.arctan2 $ fudge (v A.-. w) in
              let (tu',tv') = fixAngles tu tv in
              [CartesianArc s (fudge w) (smear r) tu' tv']


        DrawSegment s (PD x) (PD y) ->
          let Just t = lineType x y in
          case t of
            TypeI m ->
              [CartesianSegment s (fudge x) (fudge y)]

            TypeII w ->
              let Just r = A.root2 $ A.dotSq (x A.-. w) in
              let tx = A.arctan2 $ fudge (x A.-. w) in
              let ty = A.arctan2 $ fudge (y A.-. w) in
              let (tx',ty') = fixAngles tx ty in
              [CartesianArc s (fudge w) (smear r) tx' ty']


        DrawRay s (PD o) (PD a) ->
          let Just t = lineType o a in
          case t of
            TypeI m ->
              let Just (_,v) = idealPoints o a in
              [CartesianSegment s (fudge o) (fudge v)]

            TypeII w ->
              let Just (_,v) = idealPoints o a in
              let Just r = A.root2 $ A.dotSq (v A.-. w) in
              let to = A.arctan2 $ fudge (o A.-. w) in
              let tv = A.arctan2 $ fudge (v A.-. w) in
              let (to',tv') = fixAngles to tv in
              [CartesianArc s (fudge w) (smear r) to' tv']


        DrawCircle s (PD o) (PD a@(ax,ay)) ->
          let Just p@(px,py) = idealCenter o a in
          let Just r = A.root2 $ (ax - px)^2 + (ay - py)^2 in
          [CartesianCircle s (fudge p) (smear r)]



-- given angles x and y in degrees, returns angles x' and y'
-- which are coterminal to x and y (in some order) such that
-- y' - x' is in (0,180)
fixAngles :: Rational -> Rational -> (Rational, Rational)
fixAngles a b
  | 0 <= b - a && b - a <= 180 = (a, b)
  | 180 < b - a                = (b, a + 360)
  | -180 <= b - a && b - a < 0 = (b, a)
  | otherwise                  = (a, b + 360)
