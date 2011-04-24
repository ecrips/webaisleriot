var debug_text = '';

function d2(text)
{
	text = JSON.stringify(text);
	console.log(text);
}

function d(text)
{
	text = JSON.stringify(text);
	debug_text += text + "\n";
	document.getElementById('debug').innerHTML = "<pre>"+
		debug_text +"</pre>";
}

var funcNewGame = 0;
var funcButtonPressed = 1;
var funcButtonReleased = 2;
var funcButtonClicked = 3;
var funcButtonDoubleClicked = 4;
var funcGameOver = 5;
var funcGameWon = 6;
var funcGetHint = 7;
var funcGetOptions = 8;
var funcApplyOptions = 9;
var funcTimeout = 10;
var funcDroppable = 11;

var gameFunctions;

var features;
var gameScore = 0;

var slot = [];

var spacingX = 100;
var spacingY = 150;

var cardWidth = 79;
var cardHeight = 123;

var container;
var highlight;

function updateFeatures(newFeatures)
{
	features = newFeatures;
	document.getElementById("deal").style.display =
		(features & 4)?"block":"none";
}

var slotLayout =
{
	"normal":function(slot) {
		var x = slot.position[0] * spacingX;
		var y = slot.position[1] * spacingY;
		slot.size = [x,y];
		var offX = 0;
		for(var i = slot.cards.length - 1; i>=0; i--) {
			var c = slot.cards[i];
			c.style.left = x;
			c.style.top = y;
			if (offX < 10) {
				x++;
				y+=0.5;
				offX++;
			}
		}
		slot.size = slot.size.concat(x+cardWidth, y+cardHeight);
	},
	// Vertical list
	"expanded":function(slot) {
		var x = slot.position[0] * spacingX;
		var y = slot.position[1] * spacingY;
		slot.size = [x,y];
		var expand_size = slot.yExpansion*spacingY;

		for(var i = slot.cards.length - 1; i>=0; i--) {
			var c = slot.cards[i];
			c.style.left = x;
			c.style.top = y;
			y += expand_size;
		}
		slot.size = slot.size.concat(x+cardWidth, y+cardHeight);
	},
}

function Slot(param,slotid)
{
	this.type = param[0];
	this.position = param[1];
	this.cards = [];
	this.scmCards = [];
	this.yExpansion = 0.1;
	this.slotid = slotid;

	var e = document.createElement("span");
	e.className = "slot";
	e.style.left = this.position[0] * spacingX;
	e.style.top = this.position[1] * spacingY;
	this.slotSpan = e;
	container.appendChild(e);
}

function destroySlot(theSlot)
{
	removeCards(theSlot);
	container.removeChild(theSlot.slotSpan);
}

function findSlot(x,y)
{
	for(var s in slot) {
		var theSlot = slot[s];
		var size = theSlot.size;

		if (size[0] <= x && size[1] <= y &&
		    size[2] >= x && size[3] >=y) {
			return theSlot;
		}
	}
	return null;
}

function showHighlight(theSlot)
{
	var obj;
	if (theSlot.cards.length == 0) {
		obj = theSlot.slotSpan;
	} else {
		obj = theSlot.cards[0];
	}
	highlight.style.left = obj.style.left;
	highlight.style.top = obj.style.top;
	highlight.style.width = cardWidth;
	highlight.style.height = cardHeight;
	highlight.style.display = "block";
	highlight.style.zIndex = 1000;
}

function hideHighlight()
{
	highlight.style.display = "none";
}

function testGameOver()
{
	var ret = !gameFunctions[funcGameOver](mainenv, []);
	if (ret) {
		if (gameFunctions[funcGameWon](mainenv, [])) {
			alert("You won!");
		} else {
			alert("You lost!");
		}
	}
}

function doUndo()
{
	scm_apply(mainenv, ["undo"]);
}

function doRedo()
{
	scm_apply(mainenv, ["redo"]);
}

function buttonPressed(e, card, slotid, position)
{
	var scmCardlist = slot[slotid].scmCards.slice(0,position+1);
	var cardlist = slot[slotid].cards.slice(0,position+1);

	var pickup = gameFunctions[funcButtonPressed](mainenv,
		[["quote",slotid], ["quote",scmCardlist]]);

	if (pickup) {
		var startX = parseInt(card.style.left);
		var startY = parseInt(card.style.top);

		if (e.touches) e=e.targetTouches[0];

		var offsetX = startX - e.clientX;
		var offsetY = startY - e.clientY;

		var offsets = [];
		var oldzIndex = [];
		for(var i in cardlist) {
			offsets[i] = [
				parseInt(cardlist[i].style.left) - startX,
				parseInt(cardlist[i].style.top) - startY
			];
			oldzIndex[i] = cardlist[i].style.zIndex;
			cardlist[i].style.zIndex = 1000;
		}

		document.ontouchmove =
		document.onmousemove = function(e) {
			if (e.touches) e=e.targetTouches[0];

			for(var i in cardlist) {
				cardlist[i].style.left =
					e.clientX + offsetX + offsets[i][0];
				cardlist[i].style.top =
					e.clientY + offsetY + offsets[i][1];
			}

			var theSlot = findSlot(e.clientX, e.clientY);
			if (!theSlot) {
				hideHighlight();
				return;
			}
			var droppable = gameFunctions[funcDroppable](mainenv,
					[["quote",slotid],
					 ["quote",scmCardlist],
					 ["quote",theSlot.slotid]]);
			if (droppable) {
				showHighlight(theSlot);
			} else {
				hideHighlight();
			}
		}
		document.ontouchend =
		document.onmouseup = function(e) {
			if (e.touches) e=e.changedTouches[0];

			document.ontouchmove =
				document.ontouchend =
				document.onmousemove =
				document.onmouseup = null;

			hideHighlight();

			for(var i in cardlist) {
				cardlist[i].style.left =
					startX + offsets[i][0];
				cardlist[i].style.top =
					startY + offsets[i][1];
				cardlist[i].style.zIndex = oldzIndex[i];
			}

			var theSlot = findSlot(e.clientX, e.clientY);
			if (!theSlot) return;

			var oldCards = slot[slotid].scmCards;

			scm_apply(mainenv, ["record-move",
				["quote",slotid],
				["quote",slot[slotid].scmCards]]);

			slot[slotid].scmCards = oldCards.slice(cardlist.length);

			var ret = gameFunctions[funcButtonReleased](mainenv,
				[["quote",slotid],
				 ["quote",scmCardlist],
				 ["quote",theSlot.slotid]]);

			if (ret) {
				scm_apply(mainenv, ["end-move"]);
				testGameOver();
			} else {
				scm_apply(mainenv, ["discard-move"]);
				slot[slotid].scmCards = oldCards;
			}
		}
	}
}

function makeCard(details, slotid, position)
{
	var e = document.createElement("span");

	e.onmousedown = function(ev) {
		buttonPressed(ev, e, slotid, position);
		return false;
	}
	e.ontouchstart = function(ev) {
		buttonPressed(ev, e, slotid, position);
		return false;
	}

	e.className = "card";
	if (!details[2]) {
		// Face down
		offx = 2;
		offy = 4;
	} else {
		offx = details[0] - 1;
		offy = details[1];
	}
	offx *= -cardWidth;
	offy *= -cardHeight;
	e.style.backgroundPosition = offx+"px "+offy+"px";
	container.appendChild(e);
	return e;
}

function layoutSlot(slotid)
{
	var theSlot = slot[slotid];
	var layout = slotLayout[theSlot.type];

	if (!layout) {
		d("Layout "+theSlot.type+" not supported");
	}

	layout(theSlot);
}

function removeCards(theSlot)
{
	for(var i in theSlot.cards) {
		container.removeChild(theSlot.cards[i]);
	}
	theSlot.cards = [];
	theSlot.scmCards = [];
}

function updateSlot(slotid, cards)
{
	var theSlot = slot[slotid];
	removeCards(theSlot);
	theSlot.scmCards = cards;
	for(var i = cards.length - 1; i>=0; i--) {
		theSlot.cards[i] = makeCard(cards[i], slotid, i);
	}
	layoutSlot(slotid);
}

var mainenv = {
	"define": function(env, args) {
		if (typeof args[0] == "string") {
			env[args[0]] = scm_apply(env, args[1]);
		} else {
			var stmts = args.slice(1);
			var fname = args[0][0];
			var params = args[0].slice(1);
			env[fname] = function(argsenv, args) {
				args = scm_eval(argsenv, args);
				var newenv = {__parent: env};
				for(var i=0; i<params.length; i++) {
					if (params[i] == ".") {
						newenv[params[i+1]] =
							args.slice(i);
						break;
					}
					newenv[params[i]] = args[i];
				}
				return scm_eval(newenv, stmts).pop();
			}
		}
	},
	"lambda": function(env, args) {
		var stmts = args.slice(1);
		if (typeof args[0] == "string") {
			var param_name = args[0];
			return function(argsenv, args) {
				args = scm_eval(argsenv, args);
				var newenv = {__parent: env};
				newenv[param_name] = args;
				return scm_eval(newenv, stmts).pop();
			}
		}

		var params = args[0];
		return function(argsenv, args) {
			args = scm_eval(argsenv, args);
			var newenv = {__parent: env};
			for(var i=0; i<args.length; i++) {
				if (params[i] == ".") {
					newenv[params[i+1]] =
						args.slice(i);
					break;
				}
				newenv[params[i]] = args[i];
			}
			return scm_eval(newenv, stmts).pop();
		}
	},
	"+": function(env, args) {
		args = scm_eval(env, args);
		var out = 0;
		for(var i in args) {
			out += parseInt(args[i]);
		}
		return out;
	},
	"-": function(env, args) {
		args = scm_eval(env, args);
		var out = parseInt(args[0]);
		for(var i = 1; i < args.length; i++) {
			out -= parseInt(args[i]);
		}
		return out;
	},
	"*": function(env, args) {
		args = scm_eval(env, args);
		var out = 1;
		for(var i in args) {
			out *= parseInt(args[i]);
		}
		return out;
	},
	"=": function(env, args) {
		args = scm_eval(env, args);
		var ret = (parseInt(args[0]) == parseInt(args[1]));
		return ret;
	},
	">": function(env, args) {
		args = scm_eval(env, args);
		var ret = (parseInt(args[0]) > parseInt(args[1]));
		return ret;
	},
	">=": function(env, args) {
		args = scm_eval(env, args);
		var ret = (parseInt(args[0]) >= parseInt(args[1]));
		return ret;
	},
	"not": function(env, args) {
		var value = scm_apply(env, args[0]);
		return !value;
	},
	"and": function(env, args) {
		for(var i in args) {
			var val = scm_apply(env, args[i]);
			if (val != undefined && !val) {
				return false;
			}
		}
		return true;
	},
	"or": function(env, args) {
		for(var i in args) {
			var val = scm_apply(env, args[i]);
			if (val) {
				return true;
			}
		}
		return false;
	},
	"eq?": function(env, args) {
		args = scm_eval(env, args);
		var ret = (args[0] == args[1]);
		return ret;
	},
	"equal?": function(env, args) {
		args = scm_eval(env, args);
		var ret = JSON.stringify(args[0]) == JSON.stringify(args[1]);
		return ret;
	},
	"set!": function(env, args) {
		var variable = args[0];
		var value = scm_apply(env, args[1]);

		while (env[variable] == null && env.__parent) {
			env = env.__parent;
		}
		if (env[variable] == null) {
			d("Failed to find variable "+variable+" in set!");
		}
		env[variable] = value;
	},
	"quote": function(env, args) {
		return args[0];
	},
	"apply": function(env, args) {
		var optional_args = scm_apply(env, args[1]);
		args = [args[0]].concat(optional_args);
		var ret = scm_apply(env, args);
		return ret;
	},
	"if": function(env, args) {
		var cond = scm_apply(env, args[0]);
		if (cond) {
			return scm_apply(env, args[1]);
		}
		if (args[2] != null) {
			return scm_apply(env, args[2]);
		}
	},
	"cond": function(env, args) {
		for(var i in args) {
			var cond = scm_apply(env, args[i][0]);
			if (cond) {
				return scm_eval(env, args[i].slice(1)).pop();
			}
		}
	},
	"cons": function(env, args) {
		var left = scm_apply(env, args[0]);
		var right = scm_apply(env, args[1]);
		return [left].concat(right);
	},
	"list": function(env, args) {
		var ret = scm_eval(env, args);
		return ret;
	},
	"let": function(env, args) {
		var newenv = {__parent: env};
		var assignments = args[0];
		var stmts = args.slice(1);

		for(var i=0; i<assignments.length; i++) {
			var ass = assignments[i];
			newenv[ass[0]] = scm_apply(env, ass[1]);
		}

		return scm_eval(newenv, stmts).pop();
	},
	"let*": function(env, args) {
		var newenv = {__parent: env};
		var assignments = args[0];
		var stmts = args.slice(1);

		for(var i=0; i<assignments.length; i++) {
			var ass = assignments[i];
			newenv[ass[0]] = scm_apply(newenv, ass[1]);
		}
		
		return scm_eval(newenv, stmts).pop();
	},
	"list->vector": function(env, args) {
		return scm_apply(env, args[0]);
	},
	"vector-length": function(env, args) {
		var vector = scm_apply(env, args[0]);
		return vector.length;
	},
	"vector-ref": function(env, args) {
		var vector = scm_apply(env, args[0]);
		var pos = scm_apply(env, args[1]);
		return vector[pos];
	},
	"vector-set!": function(env, args) {
		var vector = scm_apply(env, args[0]);
		var pos = scm_apply(env, args[1]);
		var value = scm_apply(env, args[2]);
		vector[pos] = value;
	},
	"null?": function(env, args) {
		var value = scm_apply(env, args[0]);
		var ret = (typeof(value) == "object") && value.length == 0;
		return ret;
	},
	"car": function(env, args) {
		var list = scm_apply(env, args[0]);
		return list[0];
	},
	"cdr": function(env, args) {
		var list = scm_apply(env, args[0]);
		return list.slice(1);
	},
	"cadr": function(env, args) {
		var list = scm_apply(env, args[0]);
		return list[1];
	},
	"caddr": function(env, args) {
		var list = scm_apply(env, args[0]);
		return list[2];
	},
	"map": function(env, args) {
		if (args.length > 2) {
			d("map doesn't support more than one list");
			die();
		}
		var func = scm_apply(env, args[0]);
		var list = scm_apply(env, args[1]);
		var output = [];
		for(var i in list) {
			output.push(env,func(env, [list[i]]));
		}
		return output;
	},
	"length": function(env, args) {
		var vector = scm_apply(env, args[0]);
		return vector.length;
	},
	"begin": function(env, args) {
		return scm_eval(env, args).pop();
	},
	"reverse": function(env, args) {
		var list = scm_apply(env, args[0]);
		// Javascript's reverse affects the original array!
		return list.slice(0).reverse();
	},
	"list-tail": function(env, args) {
		var list = scm_apply(env, args[0]);
		var pos = scm_apply(env, args[1]);
		return list.slice(pos);
	},
	"member": function(env, args) {
		var item = scm_apply(env, args[0]);
		var list = scm_apply(env, args[1]);
		for(var i in list) {
			if (list[i] == item) {
				return list.slice(i);
			}
		}
		return false;
	},
	"append": function(env, args) {
		var lists = scm_eval(env, args);
		var output = [];
		for(var i in lists) {
			output = output.concat(lists[i]);
		}
		return output;
	},

	"#f": false,
	"#t": true,

	"display": function(env, args) {
		var text = scm_eval(env, args);
		d(text);
	},

	"set-lambda": function(env, args) {
		gameFunctions = scm_eval(env, args);
	},
	"set-feature-word!": function(env, args) {
		updateFeatures(scm_apply(env, args[0]));
	},
	"get-feature-word": function(env, args) {
		return features;
	},
	"reset-surface": function(env, args) {
		for(var i in slot) {
			destroySlot(slot[i]);
		}
		slot = [];
	},
	"set-statusbar-message": function(env, args) {
		var text = scm_apply(env, args[0]);
		document.getElementById("status").textContent = text;
	},
	"random": function(env, args) {
		var range = scm_apply(env, args[0]);
		var out = Math.floor(Math.random()*range);
		return out;
	},
	"add-slot": function(env, args) {
		var data = scm_apply(env, args[0]);
		var slotid = data[0];
		var cards = data[1];
		var param = data[2];

		slot[slotid] = new Slot(param,slotid);

		updateSlot(slotid, cards);
	},
	"set-slot-y-expansion!": function(env, args) {
		var slotid = scm_apply(env, args[0]);
		var expansion = scm_apply(env, args[1]);
		if (!slot[slotid]) {
			d("Unknown slot "+slotid);
		}
		slot[slotid].yExpansion = expansion;
	},
	"get-slot": function(env, args) {
		var slotid = scm_apply(env, args[0]);
		ret = [slotid, slot[slotid].scmCards];
		return ret;
	},
	"set-cards-c!": function(env, args) {
		var slotid = scm_apply(env, args[0]);
		var cards = scm_apply(env, args[1]);
		updateSlot(slotid, cards);
	},
	"get-score": function(env, args) {
		return gameScore;
	},
	"set-score!": function(env, args) {
		gameScore = scm_apply(env, args[0]);
	},
	"undo-set-sensitive": function(env, args) {
		var undo = document.getElementById("undo");
		var state = scm_apply(env, args[0]);
		undo.style.display = state?"block":"none";
	},
	"redo-set-sensitive": function(env, args) {
		var redo = document.getElementById("redo");
		var state = scm_apply(env, args[0]);
		redo.style.display = state?"block":"none";
	},

	"load-file": function(env, args) {
		var fname = scm_apply(env, args[0]);
		compile(fetchFile(fname))
	},
	"javascript": function(env, args) {
		var text = scm_apply(env, args[0]);
		return eval(text);
	},
};

function fetchFile(name)
{
	var request = new XMLHttpRequest();
	request.open("GET", name, false);
	request.send(null);
	return request.responseText;
}

function parse(text)
{
	var inComment = false;
	var inString = false;
	var output = '';

	var globallist = [];
	var curlist = globallist;
	var cursymbol = '';
	var stack = [curlist];

	function push_symbol() {
		if (cursymbol != '') {
			curlist.push(cursymbol);
		}
		cursymbol = '';
	}

	for(var c in text)
	{
		var ch = text.charAt(c);

		if (inString) {
			if (ch == '"') {
				cursymbol = ["quote",cursymbol]
				push_symbol()
				inString = false;
			} else {
				cursymbol += ch;
			}
			continue;
		} else if (inComment) {
			if (ch == "\n") {
				inComment = false;
			}
			continue;
		}

		if (ch == ";") {
			inComment = true;
			ch = " ";
		}
		if (ch == "\n" || ch == "\t" || ch == " ") {
			push_symbol();
			curlist = stack[stack.length-1];
		} else if (ch == '\'') {
			var newList = ["quote"];
			curlist.push(newList);
			curlist = newList;
		} else if (ch == '(') {
			var newList = [];
			curlist.push(newList);
			stack.push(newList);
			if (cursymbol != "") {
				d("Parse error:"+cursymbol);
			}
			cursymbol = '';
			curlist = newList;
		} else if (ch == ')') {
			push_symbol();
			stack.pop()
			curlist = stack[stack.length-1];
		} else if (ch == '"') {
			push_symbol();
			inString = true;
		} else {
			cursymbol += ch;
		}
	}
	return curlist;
}

function scm_apply(env, statement)
{
	var theenv = env;
	//d2(["apply: ",statement]);
	//d(["env: ",env]);
	if (typeof statement == "number") {
		return statement;
	}
	if (typeof statement == "string") {
		if (statement.match("^-?[0-9]+$")) {
			return parseInt(statement);
		} else if (statement.match("^-?[0-9]+\.[0-9]+$")) {
			return parseFloat(statement);
		}
		while (theenv[statement] == null && theenv.__parent) {
			theenv = theenv.__parent;
		}
		if (theenv[statement] == null) {
			d("Unknown variable:"+statement);
			return function(env, args) {
				d("Unknown function "+statement+" called");
				d({"Calling env":env});
				d({args:args});
				args = scm_eval(env, args);
				d({eval_args:args});
				die();
			}
		}
		return theenv[statement];
	}
	if (statement == null) {
		d("statement is null");
	}
	var func = scm_apply(env,statement[0]);
	if (typeof func == "function") {
		return func(env, statement.slice(1));
	} else {
		d(["Attempted to apply",func]);
		bla();
	}
}

function scm_eval(env, stmts)
{
	var output = []
	for(var line in stmts) {
		var statement = stmts[line];

		output.push(scm_apply(env, statement));
	}
	return output;
}

function compile(text)
{
	var parsed = parse(text);

	scm_eval(mainenv, parsed);
}

function makeTitle(text)
{
	var e;
	e = document.createElement("div");
	e.className = "title";
	e.textContent = text;

	return e;
}

function doOptions(env, args)
{
	var name = scm_apply(env, args[0]);
	var options = gameFunctions[funcGetOptions]();
	var optionDiv = document.getElementById("options");

	while (optionDiv.hasChildNodes()) {
		optionDiv.removeChild(optionDiv.firstChild);
	}

	optionDiv.appendChild(makeTitle("Options for "+name));

	for(var i in options) {
		var op = options[i];
		var d = document.createElement("div");
		var text = document.createElement("span");
		text.textContent = op[0];
		d.appendChild(text);
		var value = document.createElement("input");
		value.type = "checkbox";
		value.checked = op[1];
		d.appendChild(value);

		optionDiv.appendChild(d);
	}

	optionDiv.appendChild(document.createElement("p"));

	e = document.createElement("button");
	e.textContent = "Start";
	e.onclick = function() {
		optionDiv.style.display = "none";
		gameFunctions[funcNewGame](mainenv,[]);
		scm_apply(mainenv, ["start-game"]);
		gameFunctions[funcGameOver](mainenv,[]);
	}
	optionDiv.appendChild(e);
}

function chooseGame()
{
	var optionDiv = document.getElementById("options");

	var games = scm_apply(mainenv, "__game-list");

	optionDiv.appendChild(makeTitle("Choose a game:"));

	for(var i in games) {
		var name = games[i];
		var niceName = name.replace(/^(.)/, function(a)
			{return a.toUpperCase();});
		niceName = niceName.replace(/_(.)/, function(a)
			{return " "+a[1].toUpperCase();});

		name += ".scm";

		e = document.createElement("button");
		e.textContent = niceName;
		e.onclick = function() {
			scm_apply(mainenv, ["__game-options",
				["quote",name],
				["quote",niceName]]);
		}

		optionDiv.appendChild(e);
	}
}

window.onload = function() {
	container = document.getElementById("container");
	highlight = document.getElementById("highlight");

	compile(fetchFile("start.scm"));

	chooseGame();
};
