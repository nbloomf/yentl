{-# LANGUAGE FlexibleInstances #-}

module Yentl.Model.PoincarePlane (
  PoincarePlane(H), unH, interpolateH
) where

import qualified Yentl.Algebra as A
import Yentl.Geo
import Yentl.Geometry
import Yentl.View.Cartesian
import Yentl.View.Command



{--------}
{- Type -}
{--------}

data PoincarePlane = H
  { unH :: (A.ConReal, A.ConReal)
  } deriving Eq


instance Show PoincarePlane where
  show = show . unH


instance Coords PoincarePlane where
  coords a b = do
    if b <= 0
      then report $ BadCoordinates (a,b)
      else return $ H { unH = (fromRational a, fromRational b) }


{- Helpers -}

--midpointH :: (Ord a, Num a, Fractional a, A.SquareRoot a)
--  => (a,a) -> (a,a) -> (a,a)
midpointH a b =
  if a == b
    then a
    else
      let
        Just c1 = circleH a b
        Just c2 = circleH b a
        A.Only2of2 s t = A.solve2var2sol (c1, c2)
        Just (H h) = intersectLines (H a, H b) (H s, H t)
      in h

circleH :: (Ord a, Num a, Fractional a, A.SquareRoot a)
  => (a,a) -> (a,a) -> Maybe (A.CircleEquation a)
circleH o a =
  if a == o
    then Nothing
    else
      let p = modelCenterH o a in
      Just $ A.ceCenterPoint p a

tangentRayH :: (Ord a, Num a, Fractional a, A.SquareRoot a)
  => (a,a) -> (a,a) -> Maybe (a,a)
tangentRayH a@(ax,ay) b = do
  t <- lineType a b
  case t of
    TypeI _ -> Just b

    TypeII o ->
      let
        m = (ay, o-ax)
        c = (o,0)
      in
        if A.orientation a c (a A.+. m) == A.orientation a c b
          then Just $ a A.+. m
          else Just $ a A.-. m

modelCenterH :: (Ord a, Num a, Fractional a, A.SquareRoot a)
  => (a,a) -> (a,a) -> (a,a)
modelCenterH o@(ox,oy) a@(ax,ay) = case lineType o a of
  Nothing -> o
  Just (TypeI c) ->
      case compare oy ay of
        EQ -> o

        LT ->
          let
            circ = A.ceCenterRadius (c,0) oy
            A.Only2of2 (_,ty) _ = A.tangentPtCE (c,ay) circ
            ry = (ty + ay)/2
          in (c,ry)

        GT ->
          let
            Just eq = A.qeFromCoefs 1 (-2*c) (c^2 + ay^2 - oy^2)
            A.Only2of2 tx _ = A.solve1var2sol eq
            Just ell = A.lfPointFoot (c,0) (tx,ay)
            Just qy = A.solveForY ell c
            ry = (qy + ay)/2
          in (c,ry)

  Just (TypeII w) ->
      -- tangent to Type II line at a
      let Just ell = A.lfPointFoot (w,0) a in
      let Just ry = A.solveForY ell ox in
      (ox,ry)

interpolateH :: (Ord a, Num a, Fractional a, A.SquareRoot a)
  => a -> (a,a) -> (a,a) -> (a,a)
interpolateH t a@(ax,ay) b@(bx,by) =
  case lineType a b of
    Nothing -> a
    Just (TypeI c) ->
      let
        f x = case compare ay by of
          LT -> if x >= 0
            then ay + x*(by-ay)
            else ay^2 / (ay + x*(ay-by))
          GT -> if x <= 1
            then by + (x-1)*(by-ay)
            else by^2 / (by + (x-1)*(ay-by))
          EQ -> error "interpolateH (1)"
      in (c, f t)

    Just (TypeII o) ->
      let
        Just r = A.root2 $ (ax - o)^2 + ay^2
        Just ella = A.lfPointPoint (o,0) a
        Just ellb = A.lfPointPoint (o,0) b
        Just x1 = A.solveForX ella r
        Just x2 = A.solveForX ellb r
        p = A.interpolate t (x1,r) (x2,r)
        c = A.ceCenterPoint (o,0) a
        Just h = A.projectPtCE p c
      in h



{----------------------}
{- Incidence Geometry -}
{----------------------}

-- Type I:  vertical half-lines
-- Type II: circles centered on x-axis
data LineType a
  = TypeI  a -- common x coordinate
  | TypeII a -- x coordinate of ideal center
  deriving Show



-- detect the type of line that two points generate
lineType :: (Ord a, Num a, Fractional a)
  => (a,a) -> (a,a) -> Maybe (LineType a)
lineType a@(ax,ay) b@(bx,by) = do
  if a == b
    then Nothing
    else if ax == bx
      then Just (TypeI ax)
      else do
        let Just ell = A.lfPerpBisector (ax,ay) (bx,by)
        let Just o   = A.xIntercept ell
        Just (TypeII o)



instance IncidenceGeometry PoincarePlane where
  areCollinear (H a@(ax,ay)) (H b) (H c@(cx,cy)) =
    case lineType a b of
      Nothing ->
        a /= c

      Just (TypeI c_ab) ->
        cx == c_ab

      Just (TypeII o_ab) ->
        (cx - o_ab)^2 + cy^2 == (ax - o_ab)^2 + ay^2


  intersectLines (H a@(ax,ay), H b) (H u@(ux,uy), H v) = do
    type_ab <- lineType a b
    type_uv <- lineType u v
    case (type_ab, type_uv) of
      (TypeI c_ab, TypeI c_uv) -> Nothing

      (TypeI c_ab, TypeII o_uv) -> do
        y <- A.root2 $ ux^2 + uy^2 - c_ab^2 + 2*o_uv*(c_ab - ux)
        if y == 0
          then Nothing
          else Just $ H (c_ab, y)

      (TypeII o_ab, TypeI c_uv) -> do
        y <- A.root2 $ ax^2 + ay^2 - c_uv^2 + 2*o_ab*(c_uv - ax)
        if y == 0
          then Nothing
          else Just $ H (c_uv, y)

      (TypeII o_ab, TypeII o_uv) -> do
        if o_ab == o_uv
          then Nothing
          else do
            let x = (ax^2 - ux^2 + ay^2 - uy^2 + 2*(ux*o_uv - ax*o_ab))
                      / (2*(o_uv - o_ab))
            y <- A.root2 (ax^2 + ay^2 - x^2 + 2*o_ab*(x - ax))
            if y == 0
              then Nothing
              else Just $ H (x,y)



{--------------------}
{- Ordered Geometry -}
{--------------------}

instance OrderedGeometry PoincarePlane where
  isBetween (H x) (H a, H b) = case lineType a b of
    Nothing -> False

    Just (TypeI _) ->
      A.isSolutionOf2 x (A.seEndpoints a b)

    Just (TypeII o) ->
      A.orientation a b x /= A.orientation a b (o,0)


  interpolate (H a) (H b) = if a == b
    then Nothing
    else Just (H x, H y, H z)
      where
        x = interpolateH (-3/2) a b
        y = interpolateH (1/2) a b
        z = interpolateH (3/2) a b


  onSameSideOf (H (ux,uy)) (H (vx,vy)) (H a@(ax,ay), H b) =
    case lineType a b of
      Nothing -> False

      Just (TypeI c) ->
        A.signOf (ux - c) == A.signOf (vx - c)

      Just (TypeII o) ->
        let
          r2  = (ax - o)^2 + ay^2
          ru2 = (ux - o)^2 + uy^2
          rv2 = (vx - o)^2 + vy^2
        in
          case (compare ru2 r2, compare rv2 r2) of
            (LT,LT) -> True
            (GT,GT) -> True
            _       -> False



{-----------------------}
{- Congruence Geometry -}
{-----------------------}

instance CongruenceGeometry PoincarePlane where
  segmentCongruence (H (ax,ay), H (bx,by)) (H (ux,uy), H (vx,vy)) =
    let
      dab = ((bx - ax)^2 + (by - ay)^2) / (2*by*ay)
      duv = ((ux - vx)^2 + (uy - vy)^2) / (2*uy*vy)
    in dab == duv


  angleCongruence (H a, H b, H c) (H r, H s, H t) =
    let
      Just x = tangentRayH b a
      Just y = tangentRayH b c
      Just h = tangentRayH s r
      Just k = tangentRayH s t

      d = (A.dotSq $ x A.-. b) * (A.dotSq $ y A.-. b)
      e = (A.dotSq $ h A.-. s) * (A.dotSq $ k A.-. s)

      i = ((x A.-. b) `A.dot` (y A.-. b))^2 / d
      j = ((h A.-. s) `A.dot` (k A.-. s))^2 / e
    in
      if d==0 || e==0
        then d==e
        else i==j



{------------------}
{- Plane Geometry -}
{------------------}

instance PlaneGeometry PoincarePlane where
  cutCircleRay (H o, H a) (H b) = do
    if segmentCongruence (H o, H a) (H o, H b)
      then Just $ H b
      else do
        t <- lineType o b
        case t of
          TypeI c -> do
            let Just circ = circleH o a
            let Just ell = A.lfPointPoint (c,0) o
            let A.Only2of2 (_,ry) (_,sy) = A.solve2var2sol (circ,ell)
            let (_,oy) = o
            let (_,by) = b
            case compare oy by of
              EQ -> Nothing
              LT -> Just $ H (c, max ry sy)
              GT -> Just $ H (c, min ry sy)

          TypeII w -> do
            let p = modelCenterH o a
            let c1 = A.ceCenterPoint p a
            let c2 = A.ceCenterPoint (w,0) b
            let ts = A.solve2var2sol (c1, c2)
            let A.Only1of2 h = A.filterAtMost
                               (\x -> A.orientation o (w,0) b == A.orientation o (w,0) x) ts
            Just $ H h


  cutCircles (a,o,b) (x,p,y) =
    if not $ isBetween x (a,b) && isBetween b (x,y)
      then Nothing
      else do
        let
          Just c1 = circleH (unH o) (unH a)
          Just c2 = circleH (unH p) (unH x)
          A.Only2of2 s t = A.solve2var2sol (c1, c2)
        Just (H s, H t)



{--------}
{- View -}
{--------}

instance ToCartesian PoincarePlane where
  playfield = Playfield [CartesianLine dashed (0,0) (1,0)]

  toCartesian command =
    let
      smear a = A.approximateWithin (1/1000) a
      fudge (x,y) = (smear x, smear y)
    in
      case command of
        DrawPoint s (H x) ->
          [CartesianPoint s (fudge x)]


        DrawLine s (H x) (H y) ->
          let Just t = lineType x y in case t of
            TypeI c ->
              [CartesianRay s (smear c,0) (fudge x)]

            TypeII w ->
              let Just r = A.root2 $ A.dotSq ((w,0) A.-. x) in
              [CartesianArc s (smear w, 0) (smear r) 0 180]


        DrawSegment s (H x) (H y) ->
          let Just t = lineType x y in case t of
            TypeI c ->
              [CartesianSegment s (fudge x) (fudge y)]

            TypeII w ->
              let Just r = A.root2 $ A.dotSq ((w,0) A.-. x) in
              let tx = A.arctan2 $ fudge (x A.-. (w,0)) in
              let ty = A.arctan2 $ fudge (y A.-. (w,0)) in
              let a = min tx ty in
              let b = max tx ty in
              [CartesianArc s (smear w, 0) (smear r) a b]


        DrawRay s (H o) (H a) ->
          let Just t = lineType o a in case t of
            TypeI c ->
              [CartesianRay s (fudge o) (fudge a)]

            TypeII w ->
              let Just r = A.root2 $ A.dotSq ((w,0) A.-. o) in
              let to = A.arctan2 $ fudge (o A.-. (w,0)) in
              let ta = A.arctan2 $ fudge (a A.-. (w,0)) in
              if to < ta
                then [CartesianArc s (smear w, 0) (smear r) to 180]
                else [CartesianArc s (smear w, 0) (smear r) 0 to]


        DrawCircle s (H x) (H y) ->
          let p = modelCenterH x y in
          let Just r = A.root2 $ A.dotSq (p A.-. y) in
          [CartesianCircle s (fudge p) (smear r)]
