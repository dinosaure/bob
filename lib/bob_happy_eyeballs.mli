type t
(** The type of the abstract state of happy eyeballs. *)

type getaddrinfo = {
  getaddrinfo :
    'response 'a.
    'response Dns.Rr_map.key ->
    'a Domain_name.t ->
    ('response, [ `Msg of string ]) result Fiber.t;
}

val create :
  ?happy_eyeballs:Happy_eyeballs.t ->
  ?timer_interval:int64 ->
  ?getaddrinfo:getaddrinfo ->
  unit ->
  t
(** [create ~happy_eyeballs ~dns ~timer_interval ()] creates an initial state
    of happy eyeballs with the specified timeouts in nanoseconds - the default
    for [timer_interval] is [Duration.of_ms 10]. *)

val connect_host :
  t ->
  [ `host ] Domain_name.t ->
  int list ->
  (Unix.sockaddr * Unix.file_descr, [> `Msg of string ]) result Fiber.t

val connect_ip :
  t ->
  (Ipaddr.t * int) list ->
  (Unix.sockaddr * Unix.file_descr, [> `Msg of string ]) result Fiber.t

val connect :
  t ->
  [ `Inet of Unix.inet_addr * int | `Domain of [ `host ] Domain_name.t * int ] ->
  (Unix.sockaddr * Unix.file_descr, [> `Msg of string ]) result Fiber.t
(** [connect t dst] establishes a connection to [dst], which me be a host name
    [`Domain] or an IP address [`Inet]. *)

val inject : getaddrinfo -> t -> unit
(** [inject getaddrinfo t] injects a new DNS resolver into the current
    happy-eyeballs instance. By default, the happy-eyeballs instance is not
    able to resolve a domain-name. The user must inject a new implementation
    which is able to resolve a domain-name (with {!module:Bob_dns} for
    instance). Then, the user is able to use {!val:connect_host} and
    {!val:connect}. *)
