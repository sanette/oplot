module Point2 = struct

  type t = { x : float; y : float }

  let add p q = { x = p.x +. q.x; y = p.y +. q.y }
  let sub p q = { x = p.x -. q.x; y = p.y -. q.y }
  let mult s p = { x = s *. p.x; y = s *. p.y }
  let norm2 p = Float.fma p.x p.x (p.y *. p.y)
  let norm p = sqrt (norm2 p)
  let dot p q = Float.fma p.x q.x (p.y *. q.y)
  let det v1 v2 = Float.fma v1.x v2.y  (-. v2.x *. v1.y)

end

module Point3 = struct
  type t = { x : float; y : float; z : float }
end

type point = Point2.t
type point3 = Point3.t
