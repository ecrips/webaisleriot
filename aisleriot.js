// Aisleriot card games in Javascript
//
// Copyright (C) 2011-2021  Steven Price
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(function(){
"use strict";

var version = "v0.13";

var debug_text = '';

var randomSequence = [];

var seed;

function myrand()
{
	if (seed === null) {
		seed = randomSequence[1];
	}
	seed = (Math.imul(seed, 1103515245) + 12345) >>> 0;
	return (seed >>> 16);
}

function getrand(max)
{
	var val = max;
	while (val >= max) {
		val = myrand() % 64;
	}
	return val;
}

if (/Mobile/.exec(navigator.userAgent)) {
	document.documentElement.className="big";
}

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

function d_raw(text)
{
	debug_text += text;
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

var features = 2; // Score disabled
var gameScore = 0;

var gameState = "stopped";

var slot = [];

var spacingX = 100;
var spacingY = 150;

var cardWidth = 79;
var cardHeight = 123;

var container;
var highlight;

var startGameLambda;

var undo_enabled = false;

function setTextContent(element, text) {
	while (element.firstChild !== null)
		element.removeChild(element.firstChild);
	element.appendChild(document.createTextNode(text));
}

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

var statusbar_text;

function updateStatusBar(new_text)
{
	if (new_text) {
		statusbar_text = new_text;
	}
	var s = document.getElementById("status")
	setTextContent(s, statusbar_text);
	if ((features & 2) == 0) {
		var score = document.createElement("div");
		score.className = "score";
		setTextContent(score, "Score: "+gameScore);
		s.appendChild(score);
	}
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
			c.style.left = x+"px";
			c.style.top = y+"px";
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
			c.style.left = x+"px";
			c.style.top = y+"px";
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
			c.style.left = x+"px";
			c.style.top = y+"px";
			x += expand_size;
		}
		slot.size = slot.size.concat(x+cardWidth, y+cardHeight);
	},
	"partially-expanded-right":function(slot) {
		var x = slot.position[0] * spacingX;
		var y = slot.position[1] * spacingY;
		slot.size = [x,y];
		var expand_size = slot.xExpansion*spacingX;

		var start = slot.params[0];
		if (start > slot.cards.length) {
			start = slot.cards.length;
		}

		for(var i = slot.cards.length - 1; i>=0; i--) {
			var c = slot.cards[i];
			c.style.left = x+"px";
			c.style.top = y+"px";
			if (i < start) {
				x += expand_size;
			}
		}
		slot.size = slot.size.concat(x+cardWidth, y+cardHeight);
	}
};

function Slot(param,slotid)
{
	this.type = param[0];
	this.position = param[1];
	this.params = param.slice(2);
	this.cards = [];
	this.scmCards = [];
	this.yExpansion = 0.2;
	this.xExpansion = 0.2;
	this.slotid = slotid;

	var e = document.createElement("span");
	e.className = "slot";
	e.style.left = (this.position[0] * spacingX) + "px";
	e.style.top = (this.position[1] * spacingY) + "px";
	this.slotSpan = e;

	e.onclick = function(env) {
		scm_apply(mainenv, ["record-move",
				["quote",-1],
				["quote",[]]]);
		gameFunctions[funcButtonClicked](mainenv,
			[["quote", slotid]]);
		scm_apply(mainenv, ["end-move"]);
		testGameOver();
		return false;
	};

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
	for(var s=0;s<slot.length;s++) {
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
	highlight.style.width = cardWidth+"px";
	highlight.style.height = cardHeight+"px";
	highlight.style.display = "block";
	highlight.style.zIndex = 1000;
}

function hideHighlight()
{
	highlight.style.display = "none";
}

function testGameOver()
{
	function game_over_handler(result) {
		if (result == "New Game") {
			gameState = "running";
			startGameLambda();
		} else if (result == "Record win") {
			location.href = randomSequence[2];
		} else if (result == "Restart") {
			gameState = "running";
			startGameLambda();
		}
	}
	if (gameState == "gameover") {
		return;
	}
	var ret = !truth(gameFunctions[funcGameOver](mainenv, []));
	if (ret) {
		var options = ["Ok", "New Game"];
		var won = truth(gameFunctions[funcGameWon](mainenv, []));
		if (randomSequence[0] == "seed") {
			if (randomSequence.length > 2 && won) {
				options = ["Record win"];
			} else if (!won) {
				options = ["Restart"];
			}
		}
		gameState = "gameover";
		if (won) {
			dialog("You won!", options,
				game_over_handler);
		} else {
			dialog("You lost!", options,
				game_over_handler);
		}
	}
}

function doNewGame()
{
	var text = "Start new game?";
	if (randomSequence[0] == "seed") {
		text = "Restart game?";
	}
	dialog(text, ["Yes", "No"],
		function(result) {
			if (result == "Yes") {
				gameState = "running";
				startGameLambda();
			}
		});
}

function doUndo()
{
	gameState = "running";
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
		dialog("This game does not have hint support");
	} else if (hint[0] == 0) {
		dialog(hint[1]);
	} else if (hint[0] == 1 || hint[0] == 2) {
		dialog("Move "+hint[1]+" onto "+hint[2]);
	} else {
		d(hint);
	}

	return false;
}

function doDeal()
{
	if (!truth(gameFunctions[funcDealable])) {
		dialog("Deal is not available at this time");
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
	if (e.touches && e.touches.length > 1)
		return true;

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
		for(var i=0; i<cardlist.length; i++) {
			offsets[i] = [
				parseInt(cardlist[i].style.left) - startX,
				parseInt(cardlist[i].style.top) - startY
			];
			oldzIndex[i] = cardlist[i].style.zIndex;
			cardlist[i].style.zIndex = 1000;
		}

		function move(e) {
			if (!e) e = event;
			e.preventDefault();
			if (e.touches) e=e.targetTouches[0];

			for(var i=0; i<cardlist.length; i++) {
				cardlist[i].style.left = (e.clientX +
					offsetX + offsets[i][0])+"px";
				cardlist[i].style.top = (e.clientY +
					offsetY + offsets[i][1])+"px";
			}

			var x = e.pageX - container.offsetLeft;
			var y = e.pageY - container.offsetTop;
			if (!e.pageX) {
				x = e.clientX - container.offsetLeft;
				y = e.clientY - container.offsetLeft;
			}

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
		function up(e) {
			if (!e) e = event;
			if (e.touches) e=e.changedTouches[0];

			container.removeEventListener("touchmove", move);
			container.removeEventListener("touchend", up);
			container.removeEventListener("mousemove", move);
			container.removeEventListener("mouseup", up);

			hideHighlight();

			for(var i=0; i<cardlist.length; i++) {
				cardlist[i].style.left =
					(startX + offsets[i][0]) + "px";
				cardlist[i].style.top =
					(startY + offsets[i][1]) + "px";
				cardlist[i].style.zIndex = oldzIndex[i];
			}

			var x = e.pageX - container.offsetLeft;
			var y = e.pageY - container.offsetTop;
			if (!e.pageX) {
				x = e.clientX - container.offsetLeft;
				y = e.clientY - container.offsetLeft;
			}

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
		container.addEventListener("touchmove", move);
		container.addEventListener("mousemove", move);
		container.addEventListener("touchend", up);
		container.addEventListener("mouseup", up);
		return false;
	}
	return true;
}

function makeCard(details, slotid, position)
{
	var e = document.createElement("span");
	var offx, offy;

	function down(ev) {
		if (!ev) ev = event;
		return buttonPressed(ev, e, slotid, position);
	};
	e.addEventListener("mousedown", down);
	e.addEventListener("touchstart", down);
	e.onclick = function(env) {
		scm_apply(mainenv, ["record-move",
				["quote",-1],
				["quote",[]]]);
		gameFunctions[funcButtonClicked](mainenv,
			[["quote", slotid]]);
		scm_apply(mainenv, ["end-move"]);
		testGameOver();
		return false;
	};

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
	for(var i=0;i<theSlot.cards.length;i++) {
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
	for(var i=0;i<list.length;i++) {
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
			env[fname] = function(argsenv, inargs) {
				args = scm_eval(argsenv, inargs);
				var newenv = {__parent: env};
				for(var i=0; i<params.length; i++) {
					if (params[i] == ".") {
						newenv[params[i+1]] =
							args.slice(i);
						break;
					}
					if (args[i] == null) {
						d("Argument to "+fname+" is null");
						d([inargs,args]);
						d(params.length);
						d(inargs.length);
					}
					newenv[params[i]] = args[i];
				}
				return scm_eval(newenv, stmts).pop();
			};
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
			};
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
		};
	},
	"+": function(env, args) {
		args = scm_eval(env, args);
		var out = 0;
		for(var i=0;i<args.length;i++) {
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
		for(var i = 0; i < args.length; i++) {
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
	"modulo": function(env, args) {
		args = scm_eval(env, args);
		var arg1 = parseInt(args[0]);
		var arg2 = parseInt(args[1]);
		return arg1 % arg2;
	},
	"max": function(env, args) {
		args = scm_eval(env, args);
		var out = parseInt(args[0]);
		for(var i = 1; i < args.length; i++) {
			var num = parseInt(args[i]);
			if (num > out) out = num;
		}
		return out;
	},
	"min": function(env, args) {
		args = scm_eval(env, args);
		var out = parseInt(args[0]);
		for(var i = 1; i < args.length; i++) {
			var num = parseInt(args[i]);
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
		for(var i = 0; i < args.length; i++) {
			val = scm_apply(env, args[i]);
			if (!truth(val)) {
				return false;
			}
		}
		return val;
	},
	"or": function(env, args) {
		for(var i = 0; i < args.length; i++) {
			var val = scm_apply(env, args[i]);
			if (truth(val)) {
				return val;
			}
		}
		return false;
	},
	"eq?": function(env, args) {
		args = scm_eval(env, args);
		if (typeof(args[0]) == "object" &&
			typeof(args[1]) == "object" &&
			args[0].length == 0 &&
			args[1].length == 0) {
			// An empty list is equal to all other empty lists
			return true;
		}
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
		for(var i = 0; i < args.length; i++) {
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
		var v;

		// Init phase
		for(var i = 0; i < variables.length; i++) {
			v = variables[i];
			newenv[v[0]] = scm_apply(env, v[1]);
		}
		var testval = scm_apply(newenv, test[0]);
		while (!truth(testval)) {
			scm_eval(newenv, stmts);

			var oldenv = newenv;
			newenv = {__parent: env};

			// Step variables
			for(var i2 = 0; i2 < variables.length; i2++) {
				v = variables[i2];
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
			};
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
		if (list.length == 0) {
			d("Attempt to 'car' an empty list");
			d(args);
			die();
		}
		return list[0];
	},
	"cdr": function(env, args) {
		var list = scm_apply(env, args[0]);
		if (list.length == 0) {
			d("Attempt to 'cdr' an empty list");
			d(args);
			die();
		}
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
		for(var i = 0; i < list.length; i++) {
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
	"list-head": function(env, args) {
		var list = scm_apply(env, args[0]);
		var pos = scm_apply(env, args[1]);
		if(pos <= list.length && pos >= 0) {
			return list.slice(0,pos);
		} else {
			d("List-head length not within list length");
			d(args);
			die();
		}
	},
	"list-tail": function(env, args) {
		var list = scm_apply(env, args[0]);
		var pos = scm_apply(env, args[1]);
		if(pos <= list.length && pos >= 0) {
			return list.slice(pos);
		} else {
			d("List-tail length not within list length");
			d(args);
			die();
		}
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
		for(var i = 0; i< lists.length; i++) {
			output = output.concat(lists[i]);
		}
		return output;
	},
	"string-append": function(env, args) {
		var strs = scm_eval(env, args);
		var output = "";
		for(var i = 0; i < strs.length; i++) {
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

	"object->string": function(env,args) {
		var text = scm_eval(env, args);
		text = JSON.stringify(text);
		return text;
	},
	"display": function(env, args) {
		var text = scm_eval(env, args);
		d_raw(text);
	},
	"newline": function(env,args) {
		d_raw("");
		return;
	},
	"defmacro": function(env, args) {
		var name = args[0];
		var params = args[1];
		var body = args[2];

		env[name] = function(argsenv, args) {
			var realBody = body;
			for(var i = 0; i < params.length; i++) {
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

		items = items.slice(0);
		items.sort(function(b,a) {
			return less(env, [["quote",a],["quote",b]]);
		});
		return items;
	},
	"format": function(env, args) {
		var dest = scm_apply(env, args[0]);
		if (dest != false) {
			die("First argument (dest) must be '#f' in 'format'");
		}
		var format_string = scm_apply(env, args[1]);
		var options = scm_eval(env, args.slice(2));
		var fstring = /~./;
		var t;
		var text = "";
		while (t = fstring.exec(format_string)) {
			text += format_string.substr(0, t.index);
			if (t[0] == "~a") {
				text += options[0];
			} else {
				text += "<unknown "+t[0]+">";
			}
			options = options.slice(1);
			format_string = format_string.substr(t.index+t[0].length);
		}
		text += format_string;
		return text;
	},
	"integer-expt": function(env, args) {
		args = scm_eval(env, args);
		return args[0] ** args[1];
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
		for(var i = 0; i < slot.length; i++) {
			destroySlot(slot[i]);
		}
		slot = [];
	},
	"set-statusbar-message": function(env, args) {
		var text = scm_apply(env, args[0]);
		updateStatusBar(text);
	},
	"random": function(env, args) {
		var range = scm_apply(env, args[0]);
		var out;
		if (randomSequence.length == 0) {
			out = Math.floor(Math.random()*range);
		} else if (randomSequence[0] == "seed") {
			out = getrand(range);
		} else {
			out = parseInt(randomSequence.shift());
			if (out >= range) {
				d("Random sequence invalid!");
			}
		}
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
		var ret = [slotid, slot[slotid].scmCards];
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
		updateStatusBar();
	},
	"undo-set-sensitive": function(env, args) {
		var undo = document.getElementById("undo");
		var state = scm_apply(env, args[0]);
		undo_enabled = state;
		if (undo_enabled) {
			location.hash = "ingame";
		}
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
		compile(fetchFile(fname));
	},
	"__get-version": function(env, args) {
		return version;
	},
	"__do-options": doOptions
};

function fetchFile(name)
{
	var request = new XMLHttpRequest();
	request.open("GET", name, false);
	request.overrideMimeType("text/plain");
	request.send(null);
	return request.responseText;
}

function parse(text)
{
	var inComment = false;
	var inBlockComment = false;
	var inString = false;

	var globallist = [];
	var curlist = globallist;
	var cursymbol = '';
	var stack = [curlist];

	var esc = {
		"n": "\n", "a": "\u0007", "f": "\f", "r": "\r", "t": "\t",
		"v": "\v", "b": "\b", "0": "\0", "\n": ""
	};

	function push_symbol() {
		if (cursymbol != '') {
			curlist.push(cursymbol);
		}
		cursymbol = '';
	}

	for(var c=0;c<text.length;c++)
	{
		var ch = text.charAt(c);
		var newList;

		if (inString) {
			if (ch == '"') {
				cursymbol = ["quote",cursymbol];
				push_symbol();
				inString = false;
			} else if (ch == '\\') {
				c++;
				ch = text.charAt(c);
				ch = esc[ch] || ch;
				cursymbol += ch;
			} else {
				cursymbol += ch;
			}
			continue;
		}

		if (inComment) {
			if (ch == "\n") {
				inComment = false;
			}
			continue;
		}

		if (inBlockComment) {
			if ( ch == "!" && text.charAt(c+1) == "#") {
				inBlockComment = false;
				c++;
			}
			continue;
		}

		if (ch == ";") {
			inComment = true;
			ch = " ";
		}
		if (ch == "#" && text.charAt(c+1) == "!") {
			inBlockComment = true;
			c++;
			ch = " ";
		}

		if (ch == "\n" || ch == "\t" || ch == " ") {
			push_symbol();
			curlist = stack[stack.length-1];
		} else if (ch == '\'') {
			newList = ["quote"];
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
			newList = [];
			curlist.push(newList);
			stack.push(newList);
			cursymbol = '';
			curlist = newList;
		} else if (ch == ')') {
			push_symbol();
			stack.pop();
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

var number_regexp = /^-?[0-9]+(?:\.[0-9]+)?$/;

function scm_apply(env, statement)
{
	var theenv = env;
	//d2(["apply: ",statement]);
	//d(["env: ",env]);
	if (typeof statement == "number") {
		return statement;
	}
	if (typeof statement == "string") {
		if (number_regexp.exec(statement)) {
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
			};
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
		d(["Attempted to apply",func, statement]);
		die(func+" not defined");
	}
}

function scm_eval(env, stmts)
{
	var output = [];
	for(var line = 0; line < stmts.length; line++) {
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
	setTextContent(e, text);

	return e;
}

function startGame(options)
{
	var optionDiv = document.getElementById("options");

	seed = null;
	gameScore = 0;

	if (options) {
		for(var i = 0; i < options.length; i++) {
			var op = options[i];
			if (op.length == 2) {
				op[1] = op.check.checked;
			}
		}
		gameFunctions[funcApplyOptions](mainenv, [["quote",options]]);
	}

	document.getElementById("toolbar").style.display="block";

	optionDiv.style.display = "none";
	var ret = gameFunctions[funcNewGame](mainenv,[]);
	var viewport = document.getElementById("viewport");
	if (viewport) {
		var width = ret[0]*spacingX;
		var aspect = screen.width/screen.height;
		if (ret[1]*spacingY*aspect > width) {
			width = ret[1]*spacingY*aspect;
		}
		viewport.setAttribute("content","width="+width,
			false);
	}
	scm_apply(mainenv, ["start-game"]);
	testGameOver();
}

function doOptions(env, args)
{
	var name = scm_apply(env, args[0]);
	var options = gameFunctions[funcGetOptions](mainenv, []);
	var optionDiv = document.getElementById("options");

	if (!options || randomSequence.length > 0) {
		startGame();
	}

	while (optionDiv.hasChildNodes()) {
		optionDiv.removeChild(optionDiv.firstChild);
	}

	optionDiv.appendChild(makeTitle("Options for "+name));

	var radio = false;
	var radiocount = 0;
	for(var i = 0; i < options.length; i++) {
		var op = options[i];
		if (op == "begin-exclusive") {
			radio = true;
			radiocount++;
			continue;
		} else if (op == "end-exclusive") {
			radio = false;
			continue;
		}
		var d = document.createElement("div");
		var text = document.createElement("span");
		setTextContent(text, op[0]);
		d.appendChild(text);
		var value = document.createElement("input");
		if (radio) {
			value.type = "radio";
			value.name = "radio"+radiocount;
		} else {
			value.type = "checkbox";
		}
		value.checked = truth(op[1]);
		d.appendChild(value);

		optionDiv.appendChild(d);

		op.check = value;
	}

	optionDiv.appendChild(document.createElement("p"));

	var e = document.createElement("button");
	setTextContent(e, "Start");
	startGameLambda = function() {startGame(options);};
	e.onclick = startGameLambda;
	optionDiv.appendChild(e);
}

function chooseGame()
{
	var optionDiv = document.getElementById("options");

	var games = scm_apply(mainenv, "__game-list");

	optionDiv.appendChild(makeTitle("Choose a game:"));

	for(var i = 0; i < games.length; i++) {
		var name = games[i];
		var niceName = name.replace(/^(.)/, function(a)
			{return a.toUpperCase();});
		niceName = niceName.replace(/_(.)/g, function(a)
			{return " "+a[1].toUpperCase();});

		name += ".scm";

		var e = document.createElement("button");
		setTextContent(e, niceName);
		e.onclick = function(name,niceName) {return function() {
			scm_apply(mainenv, ["__game-options",
				["quote",name],
				["quote",niceName]]);
		};}(name,niceName);

		optionDiv.appendChild(e);
	}
}

function dialog(msg, buttons, callback) {
	var dialog_overlay = document.getElementById("dialog-overlay");
	var dialog_msg = document.getElementById("dialog-msg");
	dialog_msg.innerHTML = msg;

	var dialog_buttons = document.getElementById("dialog-buttons");
	while (dialog_buttons.firstChild !== null)
		dialog_buttons.removeChild(dialog_buttons.firstChild);

	if (!callback) {
		callback = function() {};
	}
	if (!buttons) {
		buttons = ["Ok"];
	}

	var firstButton = null;

	function makeButton(b) {
		var button = document.createElement("button");
		button.appendChild(document.createTextNode(b));
		button.onclick = function() {
			dialog_overlay.style.visibility = "hidden";
			while (dialog_buttons.firstChild !== null)
				dialog_buttons.removeChild(
					dialog_buttons.firstChild);
			callback(b);
		};
		dialog_buttons.appendChild(button);
		if (firstButton === null) {
			firstButton = button;
		}
	}

	buttons.forEach(function(e,i) {
		makeButton(e);
	});

	dialog_overlay.style.visibility = "visible";

	if (firstButton) {
		firstButton.focus();
	}
}

function die(msg) {
	dialog("Fatal error: "+msg);
	throw 0;
}

window.onload = function() {
	document.getElementById("newgame").onclick = doNewGame;
	document.getElementById("deal").onclick = doDeal;
	document.getElementById("undo").onclick = doUndo;
	document.getElementById("redo").onclick = doRedo;
	document.getElementById("hint").onclick = doHint;

	container = document.getElementById("container");
	highlight = document.getElementById("highlight");

	if (location.search == "?tests") {
		compile(fetchFile("tests.scm"));
		return;
	}

	compile(fetchFile("start.scm"));

	if (location.search.startsWith("?game=")) {
		randomSequence = location.search.substr(6).split(",");
		var type = randomSequence.shift();
		var new_url = location.href.substr(0,
			location.href.length - location.search.length);
		history.pushState("", type, new_url);
		scm_apply(mainenv, ["__game-options",
			["quote",type+".scm"],
			["quote",type]]);
	} else {
		chooseGame();
	}

	window.onhashchange = hash_change;
};

window.applicationCache.addEventListener("error", function(e) {
	setTextContent(document.getElementById("status"), "Failed to update offline cache");
});

function hash_change() {
	console.log("onhashchange",location.hash);
	if (location.hash != "#ingame") {
		doUndo();
		if (undo_enabled) {
			location.hash = "ingame";
		}
	}
};

})();
