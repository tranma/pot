window.onload = function() {
  var text = document.getElementById("editor");
  var from, to;
  var ws = new WebSocket("ws://127.0.0.1:9999");

  var editor = CodeMirror.fromTextArea(text, {
    lineNumbers: true,
    mode: "text/html"
  });

  editor.on("beforeChange", function (cm, chg) {
    if (chg.origin != "server") {
      from = editor.doc.indexFromPos(chg.from);
      to   = editor.doc.indexFromPos(chg.to);
    }
  });

  editor.on("change", function (cm, chg) {
    if (chg.origin != "server") {
      var str  = chg.text.join("\n");
      var ops = [];
      ops.push({"tag":"Retain","contents":from});
      if (str != "")
        ops.push({"tag":"Insert","contents":str});
      if (to > from)
        ops.push({"tag":"Delete","contents":to-from});
      var delta = {"undelta":ops};
      ws.send(JSON.stringify(delta));
    }
  });

  ws.onmessage = function(event) {
    var msg = JSON.parse(event.data);
    var index = 0;

    msg.undelta.map(function(op) {
      switch(op.tag) {
        case "Retain":
          index += op.contents;
          break;
	case "Insert":
	  var p = editor.doc.posFromIndex(index);
          console.log(op.contents, p, p);
          editor.doc.replaceRange(op.contents, p, p, "server");
	  index += op.contents.length;
	  break;
	case "Delete":
          editor.doc.replaceRange("", editor.doc.posFromIndex(index), editor.doc.posFromIndex(index + (op.contents-0)), "server");
          break;
      }
    });
  };
};
