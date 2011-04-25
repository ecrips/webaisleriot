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
var funcDealable = 12;

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

var startGameLambda;

function truth(val)
{
	return (val !== false);
}

function updateFeatures(newFeatures)
{
	features = newFeatures;
	document.getElementById("deal").style.display =
		(features & 4)?"inline-block":"none";
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
	// Horizontal list
	"expanded-right":function(slot) {
		var x = slot.position[0] * spacingX;
		var y = slot.position[1] * spacingY;
		slot.size = [x,y];
		var expand_size = slot.xExpansion*spacingX;

		for(var i = slot.cards.length - 1; i>=0; i--) {
			var c = slot.cards[i];
			c.style.left = x;
			c.style.top = y;
			x += expand_size;
		}
		slot.size = slot.size.concat(x+cardWidth, y+cardHeight);
	}
}

function Slot(param,slotid)
{
	this.type = param[0];
	this.position = param[1];
	this.cards = [];
	this.scmCards = [];
	this.yExpansion = 0.2;
	this.xExpansion = 0.2;
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
	var ret = null;
	for(var s in slot) {
		var theSlot = slot[s];
		var size = theSlot.size;

		if (size[0] <= x && size[1] <= y &&
		    size[2] >= x && size[3] >= y) {
			ret = theSlot;
		}
	}
	return ret;
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
	var ret = !truth(gameFunctions[funcGameOver](mainenv, []));
	if (ret) {
		if (truth(gameFunctions[funcGameWon](mainenv, []))) {
			alert("You won!");
		} else {
			alert("You lost!");
		}
	}
}

function doNewGame()
{
	if (confirm("Start new game?")) {
		startGameLambda();
	}
}

function doUndo()
{
	scm_apply(mainenv, ["undo"]);
	return false;
}

function doRedo()
{
	scm_apply(mainenv, ["redo"]);
	return false;
}

function doHint()
{
	var hint = gameFunctions[funcGetHint](mainenv, []);

	if (!truth(hint[0])) {
		alert("This game does not have hint support");
	} else if (hint[0] == 0) {
		alert(hint[1]);
	} else if (hint[0] == 1 || hint[0] == 2) {
		alert("Move "+hint[1]+" onto "+hint[2]);
	} else {
		d(hint);
	}

	return false;
}

function doDeal()
{
	if (!truth(gameFunctions[funcDealable])) {
		alert("Deal is not available at this time");
		return;
	}
	scm_apply(mainenv, ["record-move",
			["quote",-1],
			["quote",[]]]);
	scm_apply(mainenv, ["do-deal-next-cards"]);
	scm_apply(mainenv, ["end-move"]);
	testGameOver();
	return false;
}

function buttonPressed(e, card, slotid, position)
{
	var scmCardlist = slot[slotid].scmCards.slice(0,position+1);
	var cardlist = slot[slotid].cards.slice(0,position+1);

	var pickup = gameFunctions[funcButtonPressed](mainenv,
		[["quote",slotid], ["quote",scmCardlist]]);

	if (truth(pickup)) {
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

		container.ontouchmove =
		container.onmousemove = function(e) {
			if (e.touches) e=e.targetTouches[0];

			for(var i in cardlist) {
				cardlist[i].style.left =
					e.clientX + offsetX + offsets[i][0];
				cardlist[i].style.top =
					e.clientY + offsetY + offsets[i][1];
			}

			var x = e.clientX - container.offsetLeft;
			var y = e.clientY - container.offsetTop;

			var theSlot = findSlot(x, y);
			if (!theSlot) {
				hideHighlight();
				return;
			}
			var droppable = gameFunctions[funcDroppable](mainenv,
					[["quote",slotid],
					 ["quote",scmCardlist],
					 ["quote",theSlot.slotid]]);
			if (truth(droppable)) {
				showHighlight(theSlot);
			} else {
				hideHighlight();
			}
		}
		container.ontouchend =
		container.onmouseup = function(e) {
			if (e.touches) e=e.changedTouches[0];

			container.ontouchmove =
				container.ontouchend =
				container.onmousemove =
				container.onmouseup = null;

			hideHighlight();

			for(var i in cardlist) {
				cardlist[i].style.left =
					startX + offsets[i][0];
				cardlist[i].style.top =
					startY + offsets[i][1];
				cardlist[i].style.zIndex = oldzIndex[i];
			}

			var x = e.clientX - container.offsetLeft;
			var y = e.clientY - container.offsetTop;

			var theSlot = findSlot(x, y);
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

			if (truth(ret)) {
				scm_apply(mainenv, ["end-move"]);
				updateSlot(slotid, slot[slotid].scmCards);
				testGameOver();
			} else {
				scm_apply(mainenv, ["discard-move"]);
				slot[slotid].scmCards = oldCards;
			}
		}
		return false;
	}
	return true;
}

function makeCard(details, slotid, position)
{
	var e = document.createElement("span");

	e.onmousedown = function(ev) {
		return buttonPressed(ev, e, slotid, position);
	}
	e.ontouchstart = function(ev) {
		return buttonPressed(ev, e, slotid, position);
	}
	e.onclick = function(env) {
		scm_apply(mainenv, ["record-move",
				["quote",-1],
				["quote",[]]]);
		gameFunctions[funcButtonClicked](mainenv,
			[["quote", slotid]]);
		scm_apply(mainenv, ["end-move"]);
		testGameOver();
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

function listReplace(inlist, item, replaceWith)
{
	var list = inlist.slice(0);
	for(var i in list) {
		var e = list[i];
		if (typeof(e) == "object") {
			list[i] = listReplace(list[i], item, replaceWith);
		} else if (e == item) {
			list[i] = replaceWith;
		}
	}
	return list;
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
			out += args[i];
		}
		return out;
	},
	"-": function(env, args) {
		args = scm_eval(env, args);
		var out = args[0];
		for(var i = 1; i < args.length; i++) {
			out -= args[i];
		}
		return out;
	},
	"*": function(env, args) {
		args = scm_eval(env, args);
		var out = 1;
		for(var i in args) {
			out *= args[i];
		}
		return out;
	},
	"/": function(env, args) {
		args = scm_eval(env, args);
		var out = args[0];
		for(var i = 1; i < args.length; i++) {
			out /= args[i];
		}
		return out;
	},
	"quotient": function(env, args) {
		args = scm_eval(env, args);
		var out = parseInt(args[0]);
		for(var i = 1; i < args.length; i++) {
			out /= parseInt(args[i]);
		}
		return out;
	},
	"max": function(env, args) {
		args = scm_eval(env, args);
		var out = parseInt(args[0]);
		for(var i = 1; i < args.length; i++) {
			var num = parseInt(args[i])
			if (num > out) out = num;
		}
		return out;
	},
	"min": function(env, args) {
		args = scm_eval(env, args);
		var out = parseInt(args[0]);
		for(var i = 1; i < args.length; i++) {
			var num = parseInt(args[i])
			if (num < out) out = num;
		}
		return out;
	},
	"expt": function(env, args) {
		args = scm_eval(env, args);
		return Math.pow(parseInt(args[0]), parseInt(args[1]));
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
	"<=": function(env, args) {
		args = scm_eval(env, args);
		var ret = (parseInt(args[0]) <= parseInt(args[1]));
		return ret;
	},
	"<": function(env, args) {
		args = scm_eval(env, args);
		var ret = (parseInt(args[0]) < parseInt(args[1]));
		return ret;
	},
	"string<?": function(env, args) {
		args = scm_eval(env, args);
		var ret = (args[0] < args[1]);
		return ret;
	},
	"not": function(env, args) {
		var value = scm_apply(env, args[0]);
		return !truth(value);
	},
	"and": function(env, args) {
		var val;
		for(var i in args) {
			val = scm_apply(env, args[i]);
			if (!truth(val)) {
				return false;
			}
		}
		return val;
	},
	"or": function(env, args) {
		for(var i in args) {
			var val = scm_apply(env, args[i]);
			if (truth(val)) {
				return val;
			}
		}
		return false;
	},
	"eq?": function(env, args) {
		args = scm_eval(env, args);
		var ret = (args[0] === args[1]);
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
		if (truth(cond)) {
			return scm_apply(env, args[1]);
		}
		if (args[2] != null) {
			return scm_apply(env, args[2]);
		}
	},
	"cond": function(env, args) {
		for(var i in args) {
			var cond = scm_apply(env, args[i][0]);
			if (truth(cond)) {
				return scm_eval(env, args[i].slice(1)).pop();
			}
		}
	},
	"do": function(env, args) {
		var newenv = {__parent: env};

		var variables = args[0];
		var test = args[1];
		var stmts = args.slice(2);

		// Init phase
		for(var i in variables) {
			var v = variables[i];
			newenv[v[0]] = scm_apply(env, v[1]);
		}
		var testval = scm_apply(newenv, test[0]);
		while (!truth(testval)) {
			scm_eval(newenv, stmts);

			var oldenv = newenv;
			newenv = {__parent: env};

			// Step variables
			for(var i in variables) {
				var v = variables[i];
				if (v[2])
					newenv[v[0]] = scm_apply(oldenv, v[2]);
			}

			testval = scm_apply(newenv, test[0]);
		}
		return scm_eval(newenv, test.slice(1)).pop();
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
	"list?": function(env, args) {
		var val = scm_apply(env, args[0]);
		return (typeof(val) == "object");
	},
	"let": function(env, args) {
		var newenv = {__parent: env};
		var assignments = args[0];
		var stmts = args.slice(1);

		if (typeof(assignments) == "string") {
			// Looping let
			var name = assignments;

			assignments = stmts[0];
			stmts = stmts.slice(1);

			newenv[name] = function(argsenv, args) {
				args = scm_eval(argsenv, args);
				for(var i=0; i<assignments.length; i++) {
					var ass = assignments[i];
					newenv[ass[0]] = args[i];
				}
				
				return scm_eval(newenv, stmts).pop();
			}
		}

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
	"caar": function(env, args) {
		var list = scm_apply(env, args[0]);
		return list[0][0];
	},
	"cadr": function(env, args) {
		var list = scm_apply(env, args[0]);
		return list[1];
	},
	"cdar": function(env, args) {
		var list = scm_apply(env, args[0]);
		return list[0].slice(1);
	},
	"caaar": function(env, args) {
		var list = scm_apply(env, args[0]);
		return list[0][0][0];
	},
	"caddr": function(env, args) {
		var list = scm_apply(env, args[0]);
		return list[2];
	},
	"cadar": function(env, args) {
		var list = scm_apply(env, args[0]);
		return list[0][1];
	},
	"cadddr": function(env, args) {
		var list = scm_apply(env, args[0]);
		return list[3];
	},
	"cdaar": function(env, args) {
		var list = scm_apply(env, args[0]);
		return list[0][0].slice(1);
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
			output.push(func(env, [list[i]]));
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
	"list-ref": function(env, args) {
		var list = scm_apply(env, args[0]);
		var pos = scm_apply(env, args[1]);
		return list[pos];
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
	"string-append": function(env, args) {
		var strs = scm_eval(env, args);
		var output = "";
		for(var i in strs) {
			output += strs[i];
		}
		return output;
	},
	"number->string": function(env, args) {
		var num = scm_eval(env, args);
		return num+"";
	},

	"#f": false,
	"#t": true,

	"display": function(env, args) {
		var text = scm_eval(env, args);
		d(text);
	},
	"defmacro": function(env, args) {
		var name = args[0];
		var params = args[1];
		var body = args[2];

		env[name] = function(argsenv, args) {
			var realBody = body;
			for(var i in params) {
				realBody = listReplace(realBody,
					","+params[i], args[i]);
			}
			return scm_eval(argsenv, realBody).pop();
		};
	},
	"make-hash-table": function(env, args) {
		return {};
	},
	"hash-ref": function(env, args) {
		var table = scm_apply(env, args[0]);
		var key = scm_apply(env, args[1]);
		var def = false;
		if (args.length == 3) def = scm_apply(env, args[2]);

		var ret = table[key];
		if (ret === undefined) {
			return def;
		}
		return ret;
	},
	"hash-set!": function(env, args) {
		var table = scm_apply(env, args[0]);
		var key = scm_apply(env, args[1]);
		var val = scm_apply(env, args[2]);

		table[key] = val;
	},
	"eval": function(env, args) {
		return scm_eval(env, args).pop();
	},
	"sort": function(env, args) {
		var items = scm_apply(env, args[0]);
		var less = scm_apply(env, args[1]);

		var items = items.slice(0);
		items.sort(function(b,a) {
			return less(env, [["quote",a],["quote",b]]);
		});
		return items;
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
		undo.style.display = state?"inline-block":"none";
	},
	"redo-set-sensitive": function(env, args) {
		var redo = document.getElementById("redo");
		var state = scm_apply(env, args[0]);
		redo.style.display = state?"inline-block":"none";
	},
	"delayed-call": function(env, args) {
		var func = scm_apply(env, args[0]);
		setTimeout(function() {
			func(env, []);
			testGameOver();
		}, 100);
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
			if (cursymbol == "`") {
				// Ignore it - we're a bit dodgy with macros
			} else if (cursymbol == "#") {
				// Pretend it was a quoting symbol
				var tmpList = ["quote"];
				curlist.push(tmpList);
				curlist = tmpList;
			} else if (cursymbol != "") {
				push_symbol();
			}
			var newList = [];
			curlist.push(newList);
			stack.push(newList);
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

function startGame(options)
{
	var optionDiv = document.getElementById("options");

	if (options) {
		for(var i in options) {
			var op = options[i];
			if (op.length == 2) {
				op[1] = op.check.checked;
			}
		}
		gameFunctions[funcApplyOptions](mainenv, [["quote",options]]);
	}

	document.getElementById("toolbar").style.display="block";

	optionDiv.style.display = "none";
	gameFunctions[funcNewGame](mainenv,[]);
	scm_apply(mainenv, ["start-game"]);
	gameFunctions[funcGameOver](mainenv,[]);
}

function doOptions(env, args)
{
	var name = scm_apply(env, args[0]);
	var options = gameFunctions[funcGetOptions](mainenv, []);
	var optionDiv = document.getElementById("options");

	if (!options) {
		startGame();
	}

	while (optionDiv.hasChildNodes()) {
		optionDiv.removeChild(optionDiv.firstChild);
	}

	optionDiv.appendChild(makeTitle("Options for "+name));

	for(var i in options) {
		var op = options[i];
		if (op == "begin-exclusive" || op == "end-exclusive") {
			/* TODO: exclusive options */
			continue;
		}
		var d = document.createElement("div");
		var text = document.createElement("span");
		text.textContent = op[0];
		d.appendChild(text);
		var value = document.createElement("input");
		value.type = "checkbox";
		value.checked = truth(op[1]);
		d.appendChild(value);

		optionDiv.appendChild(d);

		op.check = value;
	}

	optionDiv.appendChild(document.createElement("p"));

	e = document.createElement("button");
	e.textContent = "Start";
	startGameLambda = function() {startGame(options);}
	e.onclick = startGameLambda;
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
		e.onclick = function(name,niceName) {return function() {
			scm_apply(mainenv, ["__game-options",
				["quote",name],
				["quote",niceName]]);
		}}(name,niceName);

		optionDiv.appendChild(e);
	}
}

window.onload = function() {
	container = document.getElementById("container");
	highlight = document.getElementById("highlight");

	compile(fetchFile("start.scm"));

	chooseGame();
};
