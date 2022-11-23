module Point2 = struct
  type t = { x : float; y : float }
end

module Point3 = struct
  type t = { x : float; y : float; z : float }
end

type point = Point2.t
type point3 = Point3.t
