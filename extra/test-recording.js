// Is included as a elm-pkg-js module when running lamdera/program-test version 4.0.0 or greater
exports.init = async function init(app)
{
    app.ports.localDevCopyToClipboard.subscribe(text => copyTextToClipboard(text));

    function copyTextToClipboard(text) {
      if (!navigator.clipboard) {
        fallbackCopyTextToClipboard(text);
        return;
      }
      navigator.clipboard.writeText(text).then(function() {
      }, function(err) {
        console.error('Error: Could not copy text: ', err);
      });
    }

    function fallbackCopyTextToClipboard(text) {
      var textArea = document.createElement("textarea");
      textArea.value = text;

      // Avoid scrolling to bottom
      textArea.style.top = "0";
      textArea.style.left = "0";
      textArea.style.position = "fixed";

      document.body.appendChild(textArea);
      textArea.focus();
      textArea.select();

      try {
        var successful = document.execCommand('copy');
        if (successful !== true) {
          console.log('Error: Copying text command was unsuccessful');
        }
      } catch (err) {
        console.error('Error: Oops, unable to copy', err);
      }

      document.body.removeChild(textArea);
    }

    var dataPath = "/tests/data/";
    var startTime = Date.now();

    var orig_XMLHttpRequest = window.XMLHttpRequest;
    var orig_open = window.XMLHttpRequest.prototype.open;

    XMLHttpRequest.prototype.open = function() {
        this.requestData = arguments;
        orig_open.apply(this, arguments);
    };

    app.ports.localDevStartRecording.subscribe(() =>
    {
        var isRecording = true;
        app.ports.localDevStopRecording.subscribe(() => { isRecording = false; })

        function sendHelper2(eventName, eventData, timeSinceStart) {
            if (!isRecording) {
                return;
            }
            let payload = { timestamp : startTime + timeSinceStart, eventType : { tag : eventName, args : eventData } };
            app.ports.localDevGotEvent.send(payload);
        }

        function keysToIgnore(event) {
            return (event.ctrlKey && event.altKey && event.key === "x"
              ) || (event.metaKey && event.altKey && event.key === "v");
        }


        // Copied from here https://github.com/GaurangTandon/checkEventAdded
        var hasEvent;
        (function (window) {
            hasEvent = function (elm, type) {
                var ev = elm.dataset.events;
                if (!ev) return false;

                return (new RegExp(type)).test(ev);
            };

            function addRemoveEvent(elm, type, bool) {
                if (bool) elm.dataset.events += "," + type;
                else elm.dataset.events = elm.dataset.events.replace(new RegExp(type), "");
            }

            function sendPointerEvent(name, event, id) {
                sendHelper2(
                    name,
                    { targetId : id
                    , ctrlKey : event.ctrlKey
                    , metaKey : event.metaKey
                    , shiftKey : event.shiftKey
                    , altKey : event.altKey
                    , clientX : event.clientX
                    , clientY : event.clientY
                    , offsetX : event.offsetX
                    , offsetY : event.offsetY
                    , pageX : event.pageX
                    , pageY : event.pageY
                    , screenX : event.screenX
                    , screenY : event.screenY
                    , button : event.button
                    , pointerType : event.pointerType
                    , pointerId : event.pointerId
                    , isPrimary : event.isPrimary
                    , width : event.width
                    , height : event.height
                    , pressure : event.pressure
                    , tiltX : event.tiltX
                    , tiltY : event.tiltY
                    }
                    , event.timeStamp);
            }

            function sendMouseEvent(name, event, id) {
                sendHelper2(
                    name,
                    { targetId : id
                    , ctrlKey : event.ctrlKey
                    , metaKey : event.metaKey
                    , shiftKey : event.shiftKey
                    , altKey : event.altKey
                    , clientX : event.clientX
                    , clientY : event.clientY
                    , offsetX : event.offsetX
                    , offsetY : event.offsetY
                    , pageX : event.pageX
                    , pageY : event.pageY
                    , screenX : event.screenX
                    , screenY : event.screenY
                    , button : event.button
                    }
                    , event.timeStamp);
            }

            function sendWheelEvent(event, id) {
                sendHelper2(
                    "Wheel",
                    { deltaX : event.deltaX
                    , deltaY : event.deltaY
                    , deltaZ : event.deltaZ
                    , deltaMode : event.deltaMode
                    , mouseEvent :
                        { targetId : id
                        , ctrlKey : event.ctrlKey
                        , metaKey : event.metaKey
                        , shiftKey : event.shiftKey
                        , altKey : event.altKey
                        , clientX : event.clientX
                        , clientY : event.clientY
                        , offsetX : event.offsetX
                        , offsetY : event.offsetY
                        , pageX : event.pageX
                        , pageY : event.pageY
                        , screenX : event.screenX
                        , screenY : event.screenY
                        , button : event.button
                        }
                    }
                    , event.timeStamp);
            }

            function sendTouchEvent(name, event, id) {
                function helper(array)
                {
                    let newArray = [];
                    for (let i = 0; i < array.length; i++) {
                        let a = array[i];
                        newArray.push({ clientX : a.clientX , clientY : a.clientY , pageX : a.pageX , pageY : a.pageY , screenX : a.screenX , screenY : a.screenY , identifier : a.identifier });
                    }
                    return newArray;
                }
                sendHelper2(
                    name,
                    { targetId : id
                    , ctrlKey : event.ctrlKey
                    , metaKey : event.metaKey
                    , shiftKey : event.shiftKey
                    , altKey : event.altKey
                    , changedTouches : helper(event.changedTouches)
                    , targetTouches : helper(event.targetTouches)
                    , touches : helper(event.touches)
                    }
                    , event.timeStamp);
            }

            let dict = new Map();

            function makeListener(name, bool) {
                var f = EventTarget.prototype[name + "EventListener"];

                return function (type, callback, capture, cb1, cb2) {
                    if (!this.dataset) this.dataset = {};
                    if (!this.dataset.events) this.dataset.events = "";

                    let newCallback = dict.get(callback);
                    if (newCallback) {

                    }
                    else
                    {
                        newCallback = (event) => {
                            if (isInsideDevBar(event.target)) {

                            }
                            else
                            {
                                switch (type)
                                {
                                    case "wheel":
                                    {
                                        if (this.id) {
                                            sendWheelEvent(event, this.id);
                                        }
                                        break;
                                    }
                                    case "input":
                                    {
                                        if (this.id) {
                                            sendHelper2("Input", { targetId : this.id, text : this.value }, event.timeStamp);
                                        }
                                        break;
                                    }
                                    case "keydown":
                                    {
                                        if (!keysToIgnore(event) && this.id) {
                                            sendHelper2("KeyDown", { targetId : this.id, ctrlKey : event.ctrlKey , metaKey : event.metaKey , shiftKey : event.shiftKey, altKey : event.altKey, key : event.key }, event.timeStamp);
                                        }
                                        break;
                                    }
                                    case "keyup":
                                    {
                                        if (!keysToIgnore(event) && this.id) {
                                            sendHelper2("KeyUp", { targetId : this.id, ctrlKey : event.ctrlKey , metaKey : event.metaKey , shiftKey : event.shiftKey, altKey : event.altKey, key : event.key }, event.timeStamp);
                                        }
                                        break;
                                    }
                                    case "focus": { if (this.id) { sendHelper2("Focus", { targetId : this.id }, event.timeStamp); } break; }
                                    case "blur": { if (this.id) { sendHelper2("Blur", { targetId : this.id }, event.timeStamp); } break; }
                                    case "pointerdown": { if (this.id) { sendPointerEvent("PointerDown", event, this.id); } break; }
                                    case "pointerup": { if (this.id) { sendPointerEvent("PointerUp", event, this.id); } break; }
                                    case "pointercancel": { if (this.id) { sendPointerEvent("PointerCancel", event, this.id); } break; }
                                    case "pointerleave": { if (this.id) { sendPointerEvent("PointerLeave", event, this.id); } break; }
                                    case "pointermove": { if (this.id) { sendPointerEvent("PointerMove", event, this.id); } break; }
                                    case "pointerover": { if (this.id) { sendPointerEvent("PointerOver", event, this.id); } break; }
                                    case "pointerenter": { if (this.id) { sendPointerEvent("PointerEnter", event, this.id); } break; }
                                    case "pointerout": { if (this.id) { sendPointerEvent("PointerOut", event, this.id); } break; }
                                    case "touchstart": { if (this.id) { sendTouchEvent("TouchStart", event, this.id); } break; }
                                    case "touchmove": { if (this.id) { sendTouchEvent("TouchMove", event, this.id); } break; }
                                    case "touchend": { if (this.id) { sendTouchEvent("TouchEnd", event, this.id); } break; }
                                    case "touchcancel": { if (this.id) { sendTouchEvent("TouchCancel", event, this.id); } break; }
                                    case "mousedown": { if (this.id) { sendMouseEvent("MouseDown", event, this.id); } break; }
                                    case "mouseup": { if (this.id) { sendMouseEvent("MouseUp", event, this.id); } break; }
                                    case "mouseleave": { if (this.id) { sendMouseEvent("MouseLeave", event, this.id); } break; }
                                    case "mousemove": { if (this.id) { sendMouseEvent("MouseMove", event, this.id); } break; }
                                    case "mouseover": { if (this.id) { sendMouseEvent("MouseOver", event, this.id); } break; }
                                    case "mouseenter": { if (this.id) { sendMouseEvent("MouseEnter", event, this.id); } break; }
                                    case "mouseout": { if (this.id) { sendMouseEvent("MouseOut", event, this.id); } break; }
                                    case "change": {
                                        if (this.type == "file") {

                                            const files = event.target.files;
                                            if (files) {
                                                const list = [];
                                                let filesLeft = files.length;
                                                const allowMultiple = this.multiple;
                                                for (var i = 0; i < files.length; i++) {
                                                    const fileItem = { name : files[i].name
                                                        , lastModified : files[i].lastModified
                                                        , contentFilePath : ""
                                                        , mimeType : files[i].type
                                                        };


                                                    list.push(fileItem);

                                                    const reader = new FileReader();
                                                    reader.addEventListener('loadend', function(loadEvent) {
                                                        filesLeft = filesLeft - 1;

                                                        bytesToSha256(loadEvent.target.result)
                                                            .then((result) => {
                                                                var path = dataPath + result.slice(0,16) + ".txt";
                                                                writeToFile(path, loadEvent.target.result);
                                                                fileItem.contentFilePath = path;
                                                                if (filesLeft === 0) {
                                                                    sendHelper2("Files", { files : list, allowMultiple : allowMultiple }, event.timeStamp);
                                                                }
                                                            });
                                                    });
                                                    reader.readAsArrayBuffer(files[i]);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            callback(event);
                        };

                        dict.set(callback, newCallback);
                    }



                    f.call(this, type, newCallback, capture);
                    addRemoveEvent(this, type, bool);
        //
        //            if (cb1) cb1();

                    return true;
                };
            }

            EventTarget.prototype.addEventListener = makeListener("add", true);
            EventTarget.prototype.removeEventListener = makeListener("remove", false);
        })();

        function writeToFile(path, content) {
            var xhr = new orig_XMLHttpRequest();
            xhr.open("POST", "/_x/write" + path, true);
            xhr.setRequestHeader("content-type", "application/octet-stream");
            xhr.send(content);
        }

        window.XMLHttpRequest = function() {
          let xhr = new orig_XMLHttpRequest();
          xhr.addEventListener('loadend', (event) =>
                {
                    if (!isRecording) {
                        return;
                    }
                    if (xhr.requestData[1].startsWith("/")) {
                        sendHelper2("HttpLocal", { filepath : xhr.requestData[1] }, event.timeStamp);
                    }
                    else
                    {
                        switch (xhr.responseType)
                        {
                            case "arraybuffer":
                                bytesToSha256(xhr.response)
                                    .then((result) => {
                                        var path = dataPath + result.slice(0,16) + ".txt";
                                        writeToFile(path, xhr.response);
                                        sendHelper2("Http", { responseType : xhr.responseType, filepath : path, request : xhr.request, method : xhr.requestData[0], url : xhr.requestData[1] }, event.timeStamp);
                                    });
                                break;
                            case "":
                                stringToSha256(xhr.response)
                                    .then((result) => {
                                        var path = dataPath + result.slice(0,16) + ".txt";
                                        writeToFile(path, xhr.response);
                                        sendHelper2("Http", { responseType : xhr.responseType, filepath : path, request : xhr.request, method : xhr.requestData[0], url : xhr.requestData[1] }, event.timeStamp);
                                    });
                                break;
                            default:
                                debugger;
                                break;
                        }
                    }
                });
          return xhr;
        };

        addEventListener("resize", (event) => {
            sendHelper2("WindowResize", { width : window.innerWidth, height : window.innerHeight }, event.timeStamp);
        });

        addEventListener("paste", (event) => {
            let targetId = getId(event.target);

            sendHelper2("Paste", { targetId : targetId, text : event.clipboardData.getData("text") }, event.timeStamp);
        });

        addEventListener("click", (event) => {
            if (!viewCheckGenerationStarted) {
                sendClickEvent("Click", event);
            }
        });

        function sendClickEvent(elmName, event) {
            if (isInsideDevBar(event.target)) {
                return;
            }
            let data = getIdOrLink(event.target);
            if (data) {
                if (data.isLink) {
                    sendHelper2("ClickLink", { path : data.path }, event.timeStamp);
                }
                else {
                    sendHelper2(elmName, { targetId : data ? data.targetId : null }, event.timeStamp);
                }
            }
        }

        function isInsideDevBar(target) {
            if (target.id === "localDev_devbar") {
                return true;
            }
            if (target.parentElement) {
                return isInsideDevBar(target.parentElement);
            }
        }

        function getId(target) {
            if (target.id && target.id !== "") {
                return target.id;
            }
            if (target.parentElement) {
                return getId(target.parentElement);
            }
            return null;
        }

        function getIdOrLink(target) {
            if (target.tagName == "A") {
                return { isLink : true, path : target.getAttribute("href") };
            }
            if (hasEvent(target, "click")) {
                return { isLink : false, targetId : target.id };
            }
            if (target.parentElement) {
                return getIdOrLink(target.parentElement);
            }
            return null;
        }

        async function stringToSha256(source) {
            const sourceBytes = new TextEncoder().encode(source);
            const digest = await crypto.subtle.digest("SHA-256", sourceBytes);
            const resultBytes = [...new Uint8Array(digest)];
            return resultBytes.map(x => x.toString(16).padStart(2, '0')).join("");
        }

        async function bytesToSha256(sourceBytes) {
            const digest = await crypto.subtle.digest("SHA-256", sourceBytes);
            const resultBytes = [...new Uint8Array(digest)];
            return resultBytes.map(x => x.toString(16).padStart(2, '0')).join("");
        }

        for (let key in app.ports) {
            switch (key)
            {
                case "localDevGotEvent": break;
                case "localDevStartRecording": break;
                case "localDevStopRecording": break;
                case "setClientId": break;
                case "setNodeTypeLeader": break;
                case "setLiveStatus": break;
                case "receive_ToBackend": break;
                case "receive_ToFrontend": break;
                case "receive_BackendModel": break;
                case "onDisconnection": break;
                case "onConnection": break;
                default: {
                    if (app.ports[key].send) {
                        let oldSend = app.ports[key].send;
                        app.ports[key].send = (a) => {
                            sendHelper2("FromJsPort", { port : key, data : JSON.stringify(a) }, Date.now() - startTime);
                            oldSend(a);
                        };
                    }
                }
            }
        }

        var viewCheckGenerationStarted = false;
        document.addEventListener(
            "keydown",
            (event) => {
                if (!isRecording) {
                    return;
                }
                if (event.metaKey && event.altKey && event.code === "KeyV" && !viewCheckGenerationStarted)
                {
                    const lastChild = document.body.lastElementChild;

                    viewCheckGenerationStarted = true;

                    let node = document.createElement("style");
                    node.textContent = "* {\n" +
                                       "     user-select: text;\n" +
                                       "     pointer-events: none;\n" +
                                       "}\n" +
                                       "\n" +
                                       "button, a, input, textarea {\n" +
                                       "    user-select: text;\n" +
                                       "    pointer-events: none;\n" +
                                       "}"
                    document.body.appendChild(node);
                    event.preventDefault();
                }
        });

        document.addEventListener(
            "keyup",
            (event) => {
                if (!isRecording) {
                    return;
                }
                if ((event.key === "Meta" || event.key === "Alt" || event.code === "KeyV") && viewCheckGenerationStarted)
                {
                    viewCheckGenerationStarted = false;
                    const lastChild = document.body.lastElementChild;

                    let selection = window.getSelection();

                    let count = selection.rangeCount;

                    let items = [];
                    for (let i = 0; i < count; i++) {
                        let range = selection.getRangeAt(i);
                        try {
                            if (range.startContainer === range.endContainer) {
                                items.push(
                                    range.startContainer.textContent.slice(
                                        Math.min(range.startOffset, range.endOffset),
                                        Math.max(range.startOffset, range.endOffset)
                                    ));
                            }
                            else {
                                items.push(range.startContainer.textContent.slice(range.startOffset));
                                items.push(range.endContainer.textContent.slice(0, range.endOffset));
                            }
                        }
                        catch (e) {
                        }
                    }
                    if (items.length > 0) {
                        sendHelper2("CheckView", { selection : items }, event.timeStamp);
                        selection.removeAllRanges();
                    }

                    if (lastChild && lastChild.tagName === 'STYLE') {
                        document.body.removeChild(lastChild);
                        event.preventDefault();
                    }
                }
        });
    });
}
