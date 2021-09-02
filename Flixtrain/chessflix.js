class Piece {
	constructor(player, icon) {
		this.player = player
		this.icon = icon
	}
}

class Rook extends Piece {
	constructor(player) {
		super(player, "&#9820;")
	}
	movement(posX, posY, board) {
		const options = []
		for (let x = posX + 1; x < 8; x++) {
			options.push([x, posY])
			if (board[x][posY] !== null) break
		}
		for (let x = posX - 1; x >= 0; x--) {
			options.push([x, posY])
			if (board[x][posY] !== null) break
		}
		for (let y = posY + 1; y < 8; y++) {
			options.push([posX, y])
			if (board[posX][y] !== null) break
		}
		for (let y = posY - 1; y >= 0; y--) {
			options.push([posX, y])
			if (board[posX][y] !== null) break
		}
		return options
	}
}

class Bishop extends Piece {
	constructor(player) {
		super(player, "&#9821;")
	}

	movement(posX, posY, board) {
		const options = []
		for (let n = 0; posX + n < 8 && posY + n < 8; n++) {
			options.push([posX + n, posY + n])
			if (board[posX + n][posY + n] !== null) break
		}
		for (let n = 0; posX - n >= 0 && posY + n < 8; n++) {
			options.push([posX - n, posY + n])
			if (board[posX - n][posY + n] !== null) break
		}
		for (let n = 0; posX + n < 8 && posY - n >= 0; n++) {
			options.push([posX + n, posY - n])
			if (board[posX + n][posY - n] !== null) break
		}
		for (let n = 0; posX - n >= 0 && posY - n >= 0; n++) {
			options.push([posX - n, posY - n])
			if (board[posX - n][posY - n] !== null) break
		}
		return options
	}
}

class Knight extends Piece {
	constructor(player) {
		super(player, "&#9822;")
	}

	movement(posX, posY, board) {
		let possible = []
		possible.push([posX+2, posY-1])
		possible.push([posX+2, posY+1])
		possible.push([posX-2, posY-1])
		possible.push([posX-2, posY+1])
		possible.push([posX+1, posY+2])
		possible.push([posX-1, posY+2])
		possible.push([posX+1, posY-2])
		possible.push([posX-1, posY-2])
		return possible.filter(coords => !coords.some(n => n > 7 || n < 0))
	}
}

class Queen extends Piece {
	constructor(player) {
		super(player, "&#9819;")
	}

	movement(posX, posY, board) {
		const options = []
		for (let x = posX + 1; x < 8; x++) {
			options.push([x, posY])
			if (board[x][posY] !== null) break
		}
		for (let x = posX - 1; x >= 0; x--) {
			options.push([x, posY])
			if (board[x][posY] !== null) break
		}
		for (let y = posY + 1; y < 8; y++) {
			options.push([posX, y])
			if (board[posX][y] !== null) break
		}
		for (let y = posY - 1; y >= 0; y--) {
			options.push([posX, y])
			if (board[posX][y] !== null) break
		}

		// Bishop 
		for (let n = 0; posX + n < 8 && posY + n < 8; n++) {
			options.push([posX + n, posY + n])
			if (board[posX + n][posY + n] !== null) break
		}
		for (let n = 0; posX - n >= 0 && posY + n < 8; n++) {
			options.push([posX - n, posY + n])
			if (board[posX - n][posY + n] !== null) break
		}
		for (let n = 0; posX + n < 8 && posY - n >= 0; n++) {
			options.push([posX + n, posY - n])
			if (board[posX + n][posY - n] !== null) break
		}
		for (let n = 0; posX - n >= 0 && posY - n >= 0; n++) {
			options.push([posX - n, posY - n])
			if (board[posX - n][posY - n] !== null) break
		}
		return options
	}
}
class King extends Piece {
	constructor(player) {
		super(player, "&#9818;")
	}
	
	// castle'ing and checks though... :(
	movement(posX, posY, board) {
		let possible = []
		possible.push([posX+1, posY])
		possible.push([posX-1, posY])
		possible.push([posX, posY+1])
		possible.push([posX, posY-1])
		possible.push([posX+1, posY+1])
		possible.push([posX+1, posY-1])
		possible.push([posX-1, posY+1])
		possible.push([posX-1, posY-1])
		return possible.filter(coords => !coords.some(n => n > 7 || n < 0))
	}
}
class Pawn extends Piece {
	constructor(player) {
		super(player, "&#9823;")
	}

	movement(posX, posY, board) {
		const options = []
		if (this.player === "black") {
			if (board[posX+1][posY] === null) {
				options.push([posX+1, posY])			
			}
			if (board[posX+2][posY] === null && posX === 6) {
				options.push([posX+2, posY])			
			}
			// Side movements
			if (board[posX+1][posY+1] !== null && posY < 7) {
				options.push([posX+1, posY+1])			
			}
			if (board[posX+1][posY-1] !== null && posY > 0) {
				options.push([posX+1, posY-1])			
			}
		}
		else if (this.player === "white") {
			if (board[posX-1][posY] === null) {
				options.push([posX-1, posY])			
			}
			if (board[posX-2][posY] === null && posX === 1) {
				options.push([posX-2, posY])			
			}
			// Side movements
			if (board[posX-1][posY+1] !== null && posY < 7) {
				options.push([posX-1, posY+1])			
			}
			if (board[posX-1][posY-1] !== null && posY > 0) {
				options.push([posX-1, posY-1])			
			}
		}
		return options
	}
}

class Board {
	constructor() {
		this.board = Array(8).fill(null).map(x => Array(8).fill(null))
		this.board[0] = [ new Rook("black"), new Knight("black"), new Bishop("black"), new Queen("black"), new King("black"), new Bishop("black"), new Knight("black"), new Rook("black")]
		this.board[1] = Array(8).fill(new Pawn("black"))
		this.board[6] = Array(8).fill(new Pawn("white"))
		this.board[7] = [ new Rook("white"), new Knight("white"), new Bishop("white"), new Queen("white"), new King("white"), new Bishop("white"), new Knight("white"), new Rook("white")]
		this.selected = [1,1]
	}

	selectedPiece() {
		return this.selected ? this.board[this.selected[0]][this.selected[1]] : null
	}

	render(fields) {
		fields.forEach((row, x) => row.forEach((field, y) => {
			const piece = this.board[x][y] 
				// remove old pieces lol
			if (piece != null) {
				const pieceDiv = document.createElement("div"); 
				pieceDiv.className = "piece " + piece.player;
				pieceDiv.innerHTML = piece.icon
				pieceDiv.addEventListener("click", () => {
					if (this.selected == [x, y]) {
						this.selected = null
					} else {
						this.selected = [x, y]
					}
					this.renderSelection(fields)
				})

				field.appendChild(pieceDiv);
			}
		}))
		this.renderSelection(fields)
	}

	renderSelection(fields) {
		const movements = this.selectedPiece().movement(this.selected[0], this.selected[1], this.board);
		movements.forEach(([x,y]) => {
			const div = document.createElement("div");
			div.className = "movement-indicator";
			div.innerHTML = "&#8226;"
			fields[x][y].appendChild(div); 
		})
	}
}

document.addEventListener("DOMContentLoaded", () => {
	const board = document.querySelector(".board")
	const fields = []
	for (let x = 0; x < 8; x++) {
		const row = [];
		for (let y = 0; y < 8; y++) {
			const div = document.createElement("div");
			div.setAttribute("pos", `${x}-${y}`);
			div.className = (x + (y % 2)) % 2 == 0 ? "bright" : "dark"
			row.push(div);
			board.appendChild(div);
		}
		fields.push(row);
	}
	const chessboard = new Board();
	

	chessboard.render(fields);
}) 
