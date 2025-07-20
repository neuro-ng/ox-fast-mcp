open Core

type notification_options = {
  prompts_changed : bool;
  resources_changed : bool;
  tools_changed : bool;
}
(** Notification options for server capabilities *)

type initialization_options = {
  notification_options : notification_options option;
  experimental_capabilities : (string, Yojson.Safe.t) Hashtbl.t option;
  additional_options : (string, Yojson.Safe.t) Hashtbl.t;
}
(** Server initialization options *)

module LowLevelServer = struct
  type t = { name : string; notification_options : notification_options }

  let create ?(name = "FastMCP Server") () =
    {
      name;
      notification_options =
        {
          prompts_changed = true;
          resources_changed = true;
          tools_changed = true;
        };
    }

  let create_initialization_options t ?(notification_options = None)
      ?(experimental_capabilities = None)
      ?(additional_options = Hashtbl.create (module String)) () =
    {
      notification_options =
        Option.value notification_options ~default:(Some t.notification_options);
      experimental_capabilities;
      additional_options;
    }

  let get_notification_options t = t.notification_options
  let get_name t = t.name
end
