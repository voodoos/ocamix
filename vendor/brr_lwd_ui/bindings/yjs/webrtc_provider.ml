type t = Jv.t

external get_web_rtc_provider : unit -> Jv.t = "get_web_rtc_provider"

let web_rtc_provider = get_web_rtc_provider ()

let _servers =
  [
    "iphone-stun.strato-iphone.de:3478";
    "numb.viagenie.ca:3478";
    "stun.12connect.com:3478";
    "stun.12voip.com:3478";
    "stun.1und1.de:3478";
    "stun.3cx.com:3478";
    "stun.acrobits.cz:3478";
    "stun.actionvoip.com:3478";
    "stun.advfn.com:3478";
    "stun.altar.com.pl:3478";
    "stun.antisip.com:3478";
    "stun.avigora.fr:3478";
    "stun.bluesip.net:3478";
    "stun.cablenet-as.net:3478";
    "stun.callromania.ro:3478";
    "stun.callwithus.com:3478";
    "stun.cheapvoip.com:3478";
    "stun.cloopen.com:3478";
    "stun.commpeak.com:3478";
    "stun.cope.es:3478";
    "stun.counterpath.com:3478";
    "stun.counterpath.net:3478";
    "stun.dcalling.de:3478";
    "stun.demos.ru:3478";
    "stun.dus.net:3478";
    "stun.easycall.pl:3478";
    "stun.easyvoip.com:3478";
    "stun.ekiga.net:3478";
    "stun.epygi.com:3478";
    "stun.etoilediese.fr:3478";
    "stun.faktortel.com.au:3478";
    "stun.freecall.com:3478";
    "stun.freeswitch.org:3478";
    "stun.freevoipdeal.com:3478";
    "stun.gmx.de:3478";
    "stun.gmx.net:3478";
    "stun.halonet.pl:3478";
    "stun.hoiio.com:3478";
    "stun.hosteurope.de:3478";
    "stun.infra.net:3478";
    "stun.internetcalls.com:3478";
    "stun.intervoip.com:3478";
    "stun.ipfire.org:3478";
    "stun.ippi.fr:3478";
    "stun.ipshka.com:3478";
    "stun.it1.hr:3478";
    "stun.ivao.aero:3478";
    "stun.jumblo.com:3478";
    "stun.justvoip.com:3478";
    "stun.l.google.com:19302";
    "stun.linphone.org:3478";
    "stun.liveo.fr:3478";
    "stun.lowratevoip.com:3478";
    "stun.lundimatin.fr:3478";
    "stun.mit.de:3478";
    "stun.miwifi.com:3478";
    "stun.modulus.gr:3478";
    "stun.myvoiptraffic.com:3478";
    "stun.netappel.com:3478";
    "stun.netgsm.com.tr:3478";
    "stun.nfon.net:3478";
    "stun.nonoh.net:3478";
    "stun.nottingham.ac.uk:3478";
    "stun.ooma.com:3478";
    "stun.ozekiphone.com:3478";
    "stun.pjsip.org:3478";
    "stun.poivy.com:3478";
    "stun.powervoip.com:3478";
    "stun.ppdi.com:3478";
    "stun.qq.com:3478";
    "stun.rackco.com:3478";
    "stun.rockenstein.de:3478";
    "stun.rolmail.net:3478";
    "stun.rynga.com:3478";
    "stun.schlund.de:3478";
    "stun.sigmavoip.com:3478";
    "stun.sip.us:3478";
    "stun.sipdiscount.com:3478";
    "stun.sipgate.net:10000";
    "stun.sipgate.net:3478";
    "stun.siplogin.de:3478";
    "stun.sipnet.net:3478";
    "stun.sipnet.ru:3478";
    "stun.sippeer.dk:3478";
    "stun.siptraffic.com:3478";
    "stun.sma.de:3478";
    "stun.smartvoip.com:3478";
    "stun.smsdiscount.com:3478";
    "stun.solcon.nl:3478";
    "stun.solnet.ch:3478";
    "stun.sonetel.com:3478";
    "stun.sonetel.net:3478";
    "stun.sovtest.ru:3478";
    "stun.srce.hr:3478";
    "stun.stunprotocol.org:3478";
    "stun.t-online.de:3478";
    "stun.tel.lu:3478";
    "stun.telbo.com:3478";
    "stun.tng.de:3478";
    "stun.twt.it:3478";
    "stun.uls.co.za:3478";
    "stun.unseen.is:3478";
    "stun.usfamily.net:3478";
    "stun.viva.gr:3478";
    "stun.vivox.com:3478";
    "stun.vo.lu:3478";
    "stun.voicetrading.com:3478";
    "stun.voip.aebc.com:3478";
    "stun.voip.blackberry.com:3478";
    "stun.voip.eutelia.it:3478";
    "stun.voipblast.com:3478";
    "stun.voipbuster.com:3478";
    "stun.voipbusterpro.com:3478";
    "stun.voipcheap.co.uk:3478";
    "stun.voipcheap.com:3478";
    "stun.voipgain.com:3478";
    "stun.voipgate.com:3478";
    "stun.voipinfocenter.com:3478";
    "stun.voipplanet.nl:3478";
    "stun.voippro.com:3478";
    "stun.voipraider.com:3478";
    "stun.voipstunt.com:3478";
    "stun.voipwise.com:3478";
    "stun.voipzoom.com:3478";
    "stun.voys.nl:3478";
    "stun.voztele.com:3478";
    "stun.webcalldirect.com:3478";
    "stun.wifirst.net:3478";
    "stun.xtratelecom.es:3478";
    "stun.zadarma.com:3478";
    "stun1.faktortel.com.au:3478";
    "stun1.l.google.com:19302";
    "stun2.l.google.com:19302";
    "stun3.l.google.com:19302";
    "stun4.l.google.com:19302";
    "stun.nextcloud.com:443";
    "relay.webwormhole.io:3478";
  ]

let make ~room_name ?signaling ?awareness yjs_doc =
  let signaling =
    Option.map (fun v -> ("signaling", Jv.of_list Jv.of_string v)) signaling
  in
  let peer_opts =
    let servers =
      List.map
        (fun s ->
          Jv.obj [| ("urls", Jv.of_string (Printf.sprintf "stun:%s" s)) |])
        [
          "stun.l.google.com:19302";
          "stun2.l.google.com:19302";
          "stun3.l.google.com:19302";
          "stun4.l.google.com:19302";
        ]
    in
    (* config: { iceServers: [{ urls: 'stun:stun.l.google.com:19302' },
       { urls: 'stun:global.stun.twilio.com:3478?transport=udp' }] },*)
    Some
      ( "peerOpts",
        Jv.obj
          [|
            ("config", Jv.obj [| ("iceServers", Jv.of_list Fun.id servers) |]);
          |] )
  in
  let () = Brr.Console.log [ peer_opts ] in
  let awareness =
    Option.map (fun a -> ("awareness", Awareness.to_jv a)) awareness
  in
  let options =
    [ signaling; peer_opts; awareness ]
    |> List.filter_map Fun.id |> Array.of_list
  in
  Jv.new' web_rtc_provider
    [| Jv.of_string room_name; Doc.Doc.to_jv yjs_doc; Jv.obj options |]

type status = { connected : bool }
type synced = { synced : bool }

type peers = {
  added : string list;
  removed : string list;
  webrtc_peers : string list;
  bc_peers : string list;
}

type 'a event =
  | Status : status event
  | Synced : synced event
  | Peers : peers event

let on (type a) t (event : a event) ~(f : a -> unit) =
  match event with
  | Status ->
      ignore
      @@ Jv.call t "on"
           [|
             Jv.of_string "status";
             Jv.callback ~arity:1 (fun status ->
                 f { connected = Jv.get status "connected" |> Jv.to_bool });
           |]
  | Synced ->
      ignore
      @@ Jv.call t "on"
           [|
             Jv.of_string "synced";
             Jv.callback ~arity:1 (fun synced ->
                 f { synced = Jv.get synced "synced" |> Jv.to_bool });
           |]
  | Peers ->
      ignore
      @@ Jv.call t "on"
           [|
             Jv.of_string "peers";
             Jv.callback ~arity:1 (fun peers ->
                 let get_array key =
                   Jv.get peers key |> Jv.to_list Jv.to_string
                 in
                 let peers =
                   {
                     added = get_array "added";
                     removed = get_array "removed";
                     webrtc_peers = get_array "webrtcPeers";
                     bc_peers = get_array "bcPeers";
                   }
                 in
                 f peers);
           |]
