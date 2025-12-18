
var fs = require('fs/promises');


var Base64Binary = {
	_keyStr: "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=",

	/* will return a  Uint8Array type */
	decodeArrayBuffer: function (input) {
		input = this.removePaddingChars(input);
		var bytes = (input.length / 4) * 3;
		var ab = new ArrayBuffer(bytes);
		this.decode(input, ab);
		return ab;
	},

	removePaddingChars: function (input) {
		var lkey = this._keyStr.indexOf(input.charAt(input.length - 1));
		var lkey2 = this._keyStr.indexOf(input.charAt(input.length - 2));
		if (lkey2 == 64 && lkey == 64) {
			return input.substring(0, input.length - 2);
		} else if (lkey == 64) {
			return input.substring(0, input.length - 1);
		}
		return input;
	},

	decode: function (input, arrayBuffer) {

		var bytes = parseInt((input.length / 4) * 3, 10);

		var uarray;
		var chr1, chr2, chr3;
		var enc1, enc2, enc3, enc4;
		var i = 0;
		var j = 0;

		if (arrayBuffer)
			uarray = new Uint8Array(arrayBuffer);
		else
			uarray = new Uint8Array(bytes);

		input = input.replace(/[^A-Za-z0-9\+\/\=]/g, "");

		for (i = 0; i < bytes; i += 3) {
			//get the 3 octects in 4 ascii chars
			enc1 = this._keyStr.indexOf(input.charAt(j++));
			enc2 = this._keyStr.indexOf(input.charAt(j++));
			enc3 = this._keyStr.indexOf(input.charAt(j++));
			enc4 = this._keyStr.indexOf(input.charAt(j++));

			chr1 = (enc1 << 2) | (enc2 >> 4);
			chr2 = ((enc2 & 15) << 4) | (enc3 >> 2);
			chr3 = ((enc3 & 3) << 6) | enc4;

			uarray[i] = chr1;
			if (enc3 != 64) uarray[i + 1] = chr2;
			if (enc4 != 64) uarray[i + 2] = chr3;
		}

		return uarray;
	}
}

function base64ToBytes(b64) {
	return new DataView(Base64Binary.decodeArrayBuffer(b64))
}


var url = 'http://localhost:8000/_x/bem';
var file = 'elm-stuff/lamdera/.lamdera-bem-dev';
var noModel = new Error('No backend model found.\nMake sure you have run your app with `lamdera live` recently.');

async function getText() {
	try {
		var res = await fetch(url);
		if (res.ok) return await res.text();
	} catch { }
	try {
		return await fs.readFile(file, 'utf8');
	} catch { }
	throw noModel;
}

(async () => {
	try {
		var bemJson = await getText();
		var bemData = JSON.parse(bemJson);

		if (!('b' in bemData)) { throw noModel; }
		var bemBytes = base64ToBytes(bemData.b);

		var app = this.Elm.Backend_Eval_.init({ flags: bemBytes });
		app.ports.log.subscribe(console.log);
	} catch (err) {
		console.error(err.message);
		process.exit(1);
	}
})();
