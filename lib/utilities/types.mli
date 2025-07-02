(** Types module for FastMCP utilities *)

module Image : sig
  type t = {
    data : bytes;
    mime_type : string;
  }
  (** Image type representing binary image data with MIME type *)

  val create : data:bytes -> mime_type:string -> t
  (** Create a new image instance *)
end 