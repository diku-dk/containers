module type unionfind = {
  type unionfind 't
  val from_array 't [n] : [n]t -> unionfind t
  val merge 't [n] : unionfind t -> [n](t, t) -> unionfind t
  val find 't [n] : unionfind t -> t -> t
}
