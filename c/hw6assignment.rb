# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
    # The constant All_My_Pieces should be declared here
    All_My_Pieces = [[[[0, 0], [-1, 0], [1, 0], [-2, 0], [2, 0]], # long long (only needs two)
                      [[0, 0], [0, -1], [0, 1], [0, -2], [0, 2]]],
                      rotations([[0, 0], [-1, 0], [1, 0], [0, -1], [-1, -1]]), # fat L
                      rotations([[0, 0], [1, 0], [0, -1]]), # smol L
                  ] + All_Pieces

    # your enhancements here
    def initialize (point_array, board)
      super(point_array, board)
      @num_pieces = point_array[0].length
    end

    def num_pieces
      @num_pieces
    end

    def self.next_piece (board)
      MyPiece.new(All_My_Pieces.sample, board)
    end

    def self.cheat_piece (board)
      MyPiece.new([[[0, 0]]], board)
    end
  
  end
  
class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @is_cheating=false
  end

  def next_piece
    if !@is_cheating
      @current_block = MyPiece.next_piece(self) 
    else
      @current_block = MyPiece.cheat_piece(self)
      @is_cheating=false
    end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    num_pieces = @current_block.num_pieces
    puts num_pieces

    (0..num_pieces - 1).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def rotate_180
    rotate_clockwise
    rotate_clockwise
    draw
  end

  def score=x
    @score = x
  end

  def cheat
    if self.score < 100 || @is_cheating
      nil
    else
      self.score -= 100
      @is_cheating = true
    end
  end
end

class MyTetris < Tetris
  # your enhancements here
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super()
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.cheat})
  end
end
  
  