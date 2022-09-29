# Bitmap Homework 4
# Alexis Kaufman
# March 23, 2022
#
# Usage Instructions: 
#         set pixel dim to 4x4
#         set display dim to 512x512
#	  set Base address for display to 0x10040000 (heap)
# Connect to MIPS and run

# set up some constants
# width of screen in pixels
# 512 / 4 = 128
.eqv WIDTH 128
# height of screen in pixels
.eqv HEIGHT 128
# move distance for arrow
.eqv ARROWMOVE	43
# mem loc
.eqv MEM 0x10040000
# color distance for arrow
.eqv COLORMOVE 18
# padding ammt
.eqv PADDING 	5

# colors
.eqv	RED 	0x00FF0000
.eqv 	ORANGE	0x00EBAB34
.eqv	GREEN 	0x0000FF00
.eqv	BLUE	0x000000FF
.eqv	YELLOW	0x00FFFF00
.eqv	MAGENTA	0x00FF00FF
.eqv 	BLACK	0x00000000
.eqv	WHITE	0x00FFFFFF

.data
rainbow:	.word	RED, ORANGE, YELLOW, GREEN, BLUE, MAGENTA
rainbowLen:	.word   6
gameBoard:	.word   0, 0, 0, 0, 0, 0, 0, 0, 0
playerOneColor: .word WHITE
playerTwoColor: .word WHITE
newLine:		.asciiz "\n"
playerOneWon:	.asciiz "Player one (X's) has won! Congrats!"
playerTwoWon:	.asciiz "Player two (O's) has won! Congrats!"
tieMessage:	.asciiz "The game has ended in a draw!"

# REGISTERS USED:
# a1-a3		used as arguments for the draw_pixel function
# t0		used as a loop counter in various functions
# t6 		holds for input
# s1 		stores address of a pixel
# s2		processes input
# s3		stores program input status:
		# 0 means top needs to be selected
		# 1 means side needs to be selected
		# 2 means color needs to be selected for player 1
		# 3 means color needs to be selected for player 2
# s4		stores player status:
		# 0 means it's player 1's turn
		# 1 means it's player 2's turn
# s5		stores TOP arrow position
		# 1 - 3, or # 1 - 6
# s6		stores SIDE arrow position
		# 1 - 3
# s7 		stores total play counter
		# 1 - 9
# t1-t4		# Used in nested arithmetic
		
		
# v0 and a0 	used for syscalls

.text

.macro draw_color_selector
	lw $t1, rainbowLen
	addi $t1, $t1, 1
	li 	$a1, WIDTH
	div 	$a1, $a1, $t1
	subi 	$a1, $a1, 3
	li 	$a2, HEIGHT  # a2 = height
	srl 	$a2, $a2, 1
	subi 	$a2, $a2, 9
	li	$a3, WHITE   # a3 = WHITE
	li $a0, 0 # down triangle
	jal draw_triangle	# draw initial down_triangle above red box in WHITE color
	
.end_macro

draw_color_options_1:
# initialize save values
	li 	$s3, 2
	li 	$s4, 0
	li 	$s5, 1
	
	jal draw_color_options
	draw_color_selector
	j check_in
	
draw_color_options_2:
	# modify save values
	li 	$s3, 3
	li 	$s4, 1
	li	$s5, 1
	
	jal clear_board
	jal draw_color_options
	draw_color_selector
	j check_in

draw_game_board:
	jal reset_player_one # change status to start game
	jal clear_board
	# start game board
	li 	$a1, WIDTH
	srl 	$a1, $a1, 1
	sub 	$a1, $a1, 2
	li 	$a2, 5  # a2 = 5
	li 	$s4, 0
	li 	$s5, 2 # middle
	jal set_a3_player_color
	
	li $a0, 0 # down triangle
	jal draw_triangle	# draw initial down_triangle at (CENTER, 5) in player color
	li $s3, 0 # Game starts with player one on top side
	li $s7, 0 # Play count starts at 0
	jal draw_down_lines
	jal draw_side_lines

	j check_in
	
##############################################

# Macros
.macro restore_pos
	move $a1, $t3 		# Restore a1
	move $a2, $t4 		# Restore a2
.end_macro

.macro restore_ra
	lw $ra, ($sp)		# obtain saved stack value
	addi $sp, $sp, 4 	# move up by 4
.end_macro

.macro save_ra
	addi $sp, $sp, -4 	# move up by 4 bytes in the stack
	sw $ra, 0($sp) 		# overwrite value at sp with word at ra
.end_macro

.macro save_pos_ra
	# Save some values
	save_ra
	move $t3, $a1
	move $t4, $a2
.end_macro

.macro restore_pos_ra
	# Restore some values
	restore_ra
	restore_pos	
.end_macro

.macro check_space_unoccupied
	mul $t5, $s5, 4
	subi $t5, $t5, 4 # t5 = 4($s5) - 4
	subi $t6, $s6, 1 
	mul $t6, $t6, 12 # t6 = ($s6 - 1)*12
	add $t7, $t5, $t6 # t7 = t5 + t6
	la $t8, gameBoard # t8 = address of spot 1,1
	add $t8, $t8, $t7 # add calculated offset
	lw $t9, ($t8) # load value at address into t9
	
	bne $t9, $0, check_in # space is occupied, return to check_in
	
.end_macro

.macro occupy_space
	mul $t5, $s5, 4
	subi $t5, $t5, 4 # t5 = 4($s5) - 4
	subi $t6, $s6, 1 
	mul $t6, $t6, 12 # t6 = ($s6 - 1)*12
	add $t7, $t5, $t6 # t7 = t5 + t6
	la $t8, gameBoard # t8 = address of spot 1,1
	add $t8, $t8, $t7 # add calculated offset
	addi $t9, $s4, 1
	sw $t9, ($t8) # load player value (1 or 2) into address at $t8
	addi $s7, $s7, 1 # increment play count
.end_macro

.macro	clear_top_arrows
	li $a0, 0
	li 	$a1, WIDTH
	srl 	$a1, $a1, 1
	sub 	$a1, $a1, 2
	li 	$a2, 5  # a2 = 5
	li 	$a3, BLACK
	jal draw_triangle
	
	li $a0, 0
	li 	$a1, WIDTH
	srl 	$a1, $a1, 1
	sub 	$a1, $a1, 2
	li 	$a2, 5  # a2 = 5
	sub	$a1, $a1, ARROWMOVE
	li 	$a3, BLACK
	jal draw_triangle
	
	li $a0, 0
	li 	$a1, WIDTH
	srl 	$a1, $a1, 1
	sub 	$a1, $a1, 2
	li 	$a2, 5  # a2 = 5
	add	$a1, $a1, ARROWMOVE
	li 	$a3, BLACK
	jal draw_triangle
.end_macro

.macro	clear_side_arrows
	li $a0, 1
	li 	$a2, HEIGHT
	srl 	$a2, $a2, 1
	sub 	$a2, $a2, 2 # a2 = middle
	li 	$a1, 5  # a1 = 5
	li 	$a3, BLACK
	jal draw_triangle
	
	li $a0, 1
	li 	$a2, HEIGHT
	srl 	$a2, $a2, 1
	sub 	$a2, $a2, 2 # a2 = middle
	li 	$a1, 5  # a1 = 5
	sub	$a2, $a2, ARROWMOVE
	li 	$a3, BLACK
	jal draw_triangle
	
	li $a0, 1
	li 	$a2, HEIGHT
	srl 	$a2, $a2, 1
	sub 	$a2, $a2, 2 # a2 = middle
	li 	$a1, 5  # a1 = 5
	add	$a2, $a2, ARROWMOVE
	li 	$a3, BLACK
	jal draw_triangle
.end_macro

##############################################

reset_player_one: # to be called with jal; resets multiple registers for player 1's beginning of turn
	li $s3, 0
	li $s4, 1	# set player to player 2
	li $s5, 2
	li $s6, 2
	jr $ra

reset_player_two: # to be called with jal; resets multiple registers for player 2's beginning of turn	
	li $s3, 0
	li $s4, 0	# set player to player 1
	li $s5, 2	# reset top arrow to middle
	li $s6, 2	# reset side arrow to middle
	jr $ra

##############################################

set_a3_player_color: # to be called with jal;	sets a3 to respective color for current player
	bne $s4, $zero, player_two_a3
	lw $a3, playerOneColor
	jr $ra

player_two_a3:
	lw $a3, playerTwoColor
	jr $ra

##############################################

check_winner: # to be called with jal
	save_pos_ra
	la $t3, gameBoard # t3 = address of first grid point
	lw $t4, ($t3) # t4 = value at first checkpoint, initally first grid pt
	li $t5, 0 
	li $t6, 0
	j check_verticals
	
.macro calc_win_helper (%depth_value)
	mul $t8, $t6, %depth_value # t8 = depth * depth_value
	add $t7, $t5, $t8 # t7 = starting value + calculated offset
	add $t9, $t3, $t7 # t9 = original memory location + t7
	lw $t9, ($t9) # load value of address at t9 into t9
.end_macro

return_to_loop:
	jr $ra

##############################################

check_verticals:
	jal check_verticals_loop # with initial vals
	addi $t5, $t5, 4
	lw $t4, 4($t3)
	li $t6, 0
	jal check_verticals_loop
	addi $t5, $t5, 4
	lw $t4, 8($t3)
	li $t6, 0
	jal check_verticals_loop
	j check_horizontals

# $t5 = starting value for top column (0, 4, 8)
# $t6 = current height (0, 1, 2)
check_verticals_loop:
	beq $t6, 3, full_match_found
	calc_win_helper (12)
	bne $t9, $t4, return_to_loop # different found
	beq $t9, $zero, return_to_loop # empty found

	addi $t6, $t6, 1
	j check_verticals_loop

#############################################

check_horizontals:
	li $t5, 0 
	li $t6, 0
	jal check_horizontal_loop # with initial vals
	addi $t5, $t5, 12
	lw $t4, 12($t3)
	li $t6, 0
	jal check_horizontal_loop
	addi $t5, $t5, 12
	lw $t4, 24($t3)
	li $t6, 0
	jal check_horizontal_loop
	j check_diagonals

# $t5 = starting value for side column (0, 12, 24)
# $t6 = current depth (0, 1, 2)
check_horizontal_loop:
	beq $t6, 3, full_match_found
	calc_win_helper (4)
	bne $t9, $t4, return_to_loop # different found
	beq $t9, $zero, return_to_loop # empty found

	addi $t6, $t6, 1
	j check_horizontal_loop

return_to_horizontals:
	jr $ra

#############################################

check_diagonals:
	li $t5, 0 
	lw $t4, 0($t3)
	li $t6, 0
	jal check_left_diagonals_loop # with initial vals
	li $t5, 8
	lw $t4, 8($t3)
	li $t6, 0
	jal check_right_diagonals_loop
	j done_checking_winner

# $t5 = starting value for top left spot (0, 0)
# $t6 = current depth (0, 1, 2)
check_left_diagonals_loop:
	beq $t6, 3, full_match_found
	calc_win_helper (16)
	bne $t9, $t4, return_to_loop # different found
	beq $t9, $zero, return_to_loop # empty found

	addi $t6, $t6, 1
	j check_left_diagonals_loop
	
# $t5 = starting value for top right spot (0, 8)
# $t6 = current depth (0, 1, 2)
check_right_diagonals_loop:
	beq $t6, 3, full_match_found
	calc_win_helper (8)
	bne $t9, $t4, return_to_loop # different found
	beq $t9, $zero, return_to_loop # empty found

	addi $t6, $t6, 1
	j check_right_diagonals_loop

return_to_diagonals:
	jr $ra

#############################################

done_checking_winner:
	beq $s7, 9, game_tied
	j done_return_nested

full_match_found: #END GAME
	beq $t9, 1, player_one_won
	beq $t9, 2, player_two_won
	j exit # if error occurs

player_one_won:
	li $v0, 55
	la $a0, playerOneWon
	li $a1, 1
	syscall
	j exit

player_two_won:
	li $v0, 55
	li $a1, 1
	la $a0, playerTwoWon
	syscall
	j exit

game_tied:
	li $v0, 55
	li $a1, 1
	la $a0, tieMessage
	syscall
	j exit


##############################################

check_in:
# check for input
	lw $t6, 0xffff0000  #t6 holds if input available
	beq $t6, 0, check_in   #If no input, keep displaying
	
# process input
	lw 	$s2, 0xffff0004
	beq	$s2, 32, process_chosen # input space
	beq	$s2 119, up 	# input w
	beq	$s2, 115, down 	# input s
	beq	$s2, 97, left  	# input a
	beq	$s2, 100, right	# input d
	beq	$s2, 27, exit   # input esc
	j check_in

process_chosen:
	beq $s3, 2, player_one_color_chosen
	beq $s3, 3, player_two_color_chosen
	
	# logic to determine if it is player one's turn, and they have selected top side
	seq $t1, $s4, 0
	seq $t2, $s3, $zero
	and $t3, $t1, $t2
	beq $t3, 1, player_to_side
	
	# logic to determine if it is player one's turn, and they have selected the side side
	seq $t1, $s4, 0
	seq $t2, $s3, 1
	and $t3, $t1, $t2
	beq $t3, 1, player_one_chosen_spot
	
	# logic to determine if it is player two's turn, and they have selected the top side
	seq $t1, $s4, 1
	seq $t2, $s3, 0
	and $t3, $t1, $t2
	beq $t3, 1, player_to_side
	
	# logic to determine if it is player two's turn, and they have selected the side side
	seq $t1, $s4, 1
	seq $t2, $s3, 1
	and $t3, $t1, $t2
	beq $t3, 1, player_two_chosen_spot
	
	j check_in
	
player_to_side:
	li $s3, 1 # set side needs to be selected
	li $a0, 1 # right pointing arrow
	li 	$a2, HEIGHT
	srl 	$a2, $a2, 1
	sub 	$a2, $a2, 2 # a2 = middle
	li 	$a1, 5  # a1 = 5
	jal set_a3_player_color
	jal draw_triangle
	li $s6, 2 # set arrow position marker to middle
	j check_in
	
player_to_top: 
	li 	$a1, WIDTH
	srl 	$a1, $a1, 1
	sub 	$a1, $a1, 2
	li 	$a2, 5  # a2 = 5
	li 	$s5, 2 # middle
	jal set_a3_player_color
	li $a0, 0 # down triangle
	jal draw_triangle	# draw initial down_triangle at (CENTER, 5) in player color
	
	j check_in

player_one_chosen_spot:
	check_space_unoccupied
	occupy_space
	li 	$a2, HEIGHT
	srl 	$a2, $a2, 1 # a2 = middle height
	subi $t5, $s6, 2
	mul $t5, $t5, ARROWMOVE
	add	$a2, $a2, $t5
	li 	$a1, WIDTH
	srl 	$a1, $a1, 1 # a2 = middle width
	subi $t5, $s5, 2
	mul $t5, $t5, ARROWMOVE
	add	$a1, $a1, $t5
	
	jal set_a3_player_color
	jal draw_centered_x
	jal reset_player_one
	clear_top_arrows
	clear_side_arrows
	jal check_winner
	j player_to_top
	
player_two_chosen_spot: 
	check_space_unoccupied
	occupy_space
	li 	$a2, HEIGHT
	srl 	$a2, $a2, 1 # a2 = middle height
	subi $t5, $s6, 2
	mul $t5, $t5, ARROWMOVE
	add	$a2, $a2, $t5
	li 	$a1, WIDTH
	srl 	$a1, $a1, 1 # a2 = middle width
	subi $t5, $s5, 2
	mul $t5, $t5, ARROWMOVE
	add	$a1, $a1, $t5
	
	jal set_a3_player_color
	jal draw_centered_o
	jal reset_player_two
	clear_top_arrows
	clear_side_arrows
	jal check_winner
	j player_to_top

up: 
	beq $s3, $0, check_in
	beq $s6, 1, check_in
	li $a3, BLACK
	li $a0, 1 # right triangle
	jal draw_triangle
	addi $a2, $a2, -ARROWMOVE
	addi $s6, $s6, -1 # update s6 to move one spot up
	jal set_a3_player_color
	li $a0, 1 # right triangle
	jal draw_triangle
	j check_in

down: 
	beq $s3, $0, check_in
	beq $s6, 3, check_in
	li $a3, BLACK
	li $a0, 1 # right triangle
	jal draw_triangle
	addi $a2, $a2, ARROWMOVE
	addi $s6, $s6, 1 # update s6 to move one spot down
	jal set_a3_player_color
	li $a0, 1 # right triangle
	jal draw_triangle
	j check_in

left: 
	beq $s3, 2, color_left
	beq $s3, 3, color_left
	beq $s5, 1, check_in
	bne $s3, $0, check_in
	li $a3, BLACK
	li $a0, 0 # down triangle
	jal draw_triangle
	addi $a1, $a1, -ARROWMOVE
	addi $s5, $s5, -1 # update s5 to move one spot left
	jal set_a3_player_color
	li $a0, 0 # down triangle
	jal draw_triangle
	j check_in

right: 
	beq $s3, 2, color_right
	beq $s3, 3, color_right
	beq $s5, 3, check_in
	bne $s3, $0, check_in
	li $a3, BLACK
	li $a0, 0 # down triangle
	jal draw_triangle
	addi $a1, $a1, ARROWMOVE
	addi $s5, $s5, 1 # update s5 to move one spot right
	jal set_a3_player_color
	li $a0, 0 # down triangle
	jal draw_triangle
	j check_in
	
color_left:
	beq $s5, 1, check_in
	li $a3, BLACK
	li $a0, 0 # down triangle
	jal draw_triangle
	addi $a1, $a1, -COLORMOVE
	addi $s5, $s5, -1 # update s5 to move one spot left
	li $a3, WHITE
	li $a0, 0 # down triangle
	jal draw_triangle
	j check_in

color_right:
	beq $s5, 6, check_in
	li $a3, BLACK
	li $a0, 0 # down triangle
	jal draw_triangle
	addi $a1, $a1, COLORMOVE
	addi $s5, $s5, 1 # update s5 to move one spot left
	li $a3, WHITE
	li $a0, 0 # down triangle
	jal draw_triangle
	j check_in
	
player_one_color_chosen:
	la $t5, rainbow
	addi $t6, $s5, -1
	mul $t6, $t6, 4
	add $t5, $t6, $t5
	lw $t6, ($t5)
	sw $t6, playerOneColor
	
	j draw_color_options_2
	
player_two_color_chosen:
	la $t5, rainbow
	addi $t6, $s5, -1
	mul $t6, $t6, 4
	add $t5, $t6, $t5
	lw $t6, ($t5)
	lw $t7, playerOneColor
	beq $t7, $t6, check_in
	sw $t6, playerTwoColor
	
	j draw_game_board
######################################

# draw a 5 x 3 triangle, starting at given x and y
# $a0 = direction to draw triangle; 
	# 0 = down
	# 1 = right
# $a1 = x
# $a2 = y
# $a3 = color	


draw_triangle:
	save_pos_ra
	li $t0, 0
# start looping
	beq $a0, $zero, down_triangle_loop_top
	bne $a0, $zero, right_triangle_loop_top

#############################################

# down-pointing triangle functions

reset_middle_helper_down:
	subi $a1, $a1, 5
	li $t0, 1
	addi $a2, $a2, 1
	j down_triangle_loop_middle

reset_bottom_helper_down:
	subi $a1, $a1, 3
	li $t0, 3
	addi $a2, $a2, 1
	j down_triangle_loop_bottom

###

done_drawing_down_triangle:
# return
	restore_pos_ra
	jr $ra

# $t0 = current length
down_triangle_loop_top:
	jal draw_pixel
	addi $a1, $a1, 1
	beq $t0, 4, reset_middle_helper_down
	addi $t0, $t0, 1
	j down_triangle_loop_top

# $t0 = current length
down_triangle_loop_middle:
	addi $a1, $a1, 1
	jal draw_pixel
	beq $t0, 3, reset_bottom_helper_down
	addi $t0, $t0, 1
	j down_triangle_loop_middle

down_triangle_loop_bottom:
	addi $a1, $a1, 2
	jal draw_pixel
	j done_drawing_down_triangle
######################################

# right-pointing triangle functions

reset_middle_helper_right:
	subi $a2, $a2, 5
	li $t0, 1
	addi $a1, $a1, 1
	j right_triangle_loop_middle

reset_bottom_helper_right:
	subi $a2, $a2, 3
	li $t0, 3
	addi $a1, $a1, 1
	j right_triangle_loop_bottom

###

done_drawing_right_triangle:
# return
	restore_pos_ra
	jr $ra

# $t0 = current length
right_triangle_loop_top:
	jal draw_pixel
	addi $a2, $a2, 1
	beq $t0, 4, reset_middle_helper_right
	addi $t0, $t0, 1
	j right_triangle_loop_top

# $t0 = current length
right_triangle_loop_middle:
	addi $a2, $a2, 1
	jal draw_pixel
	beq $t0, 3, reset_bottom_helper_right
	addi $t0, $t0, 1
	j right_triangle_loop_middle

right_triangle_loop_bottom:
	addi $a2, $a2, 2
	jal draw_pixel
	j done_drawing_right_triangle


######################################

# draw the games's vertical borders
# $t0 = current length
# $t1 = length to draw
# $t2 = temporary arithmetic operand
# $t3 = previous x
# $t4 = previous y

draw_down_lines:
	save_pos_ra

# Set up arguments for draw_pixel
	li $a1, WIDTH
	div $a1, $a1, 3
	li $a2, PADDING
	li $a3, WHITE

# Set up arguments for left line
	li $t0, 0
	li $t1, HEIGHT
	li $t2, PADDING
	mul $t2, $t2, 2
	sub $t1, $t1, $t2
	jal draw_vertical_line

# Set up arguments for right line
	li $t0, 0
	li $a1, WIDTH
	mul $a1, $a1, 2
	div $a1, $a1, 3
	li $a2, PADDING
	jal draw_vertical_line

# Restore values
	restore_pos
	j done_return_nested

draw_vertical_line:
	save_ra
	j loop_vertical_line

loop_vertical_line:
	bge $t0, $t1, done_return_nested
	jal draw_pixel
	addi $a2, $a2, 1
	addi $t0, $t0, 1
	j loop_vertical_line

######################################

# draw the game's horizontal borders
# $t0 = current length
# $t1 = length to draw
# $t2 = temporary arithmetic operand
# $t3 = previous x
# $t4 = previous y

draw_side_lines:
# Save some values
	save_pos_ra

# Set up arguments for draw_pixel
	li $a1, PADDING
	li $a2, HEIGHT
	div $a2, $a2, 3
	li $a3, WHITE

# Set up arguments for top line
	li $t0, 0
	li $t1, WIDTH
	li $t2, PADDING
	mul $t2, $t2, 2
	sub $t1, $t1, $t2
	jal draw_line_horizontal

# Set up arguments for bottom line
	li $t0, 0
	li $a2, HEIGHT
	mul $a2, $a2, 2
	div $a2, $a2, 3
	li $a1, PADDING
	jal draw_line_horizontal

# Restore values
	restore_pos
	j done_return_nested

draw_line_horizontal:
	save_ra
	j loop_horizontal_line

loop_horizontal_line:
	bge $t0, $t1, done_return_nested
	jal draw_pixel
	addi $a1, $a1, 1
	addi $t0, $t0, 1
	j loop_horizontal_line
	
######################################

# t0 - iteration number
# t1 - times to iterate
# t2 - address of first array value
# t3 - previous x to save
# t4 - previous y to save
# t5-t6 - used in arithmetic
# $t7 - holds color NOT to draw

draw_color_options:
	li $t0, 1
	lw $t1, rainbowLen
	addi $t1, $t1, 1
	la $t2, rainbow
	
	# save values
	save_pos_ra
	
	j draw_rainbow_loop
	
draw_rainbow_loop:
	# check condition:
	beq $t0, $t1, done_return_nested

	# determine y value
	li $a2, HEIGHT
	srl $a2, $a2, 1
	
	# determine x value
	li $t5, WIDTH
	div $t5, $t5, $t1
	mul $t5, $t5, $t0
	move $a1, $t5
	
	# determine color value
	lw $a3, ($t2)
	addi $t2, $t2, 4
	
	# increment values
	addi $t0, $t0, 1
	
	lw $t7, playerOneColor
	
	beq $t7, $a3, draw_rainbow_loop
	
	# draw pixels
	jal draw_centered_square
	
	j draw_rainbow_loop
######################################

# Draws a centered square at current x,y with a size of 5 x 5.

# Should be called with a jal

# t6 = top iterator (-2 to 2)
# t7 = side iterator (-2 to 2)
# t5 = used in arithmetic

draw_centered_square:
	save_pos_ra	# save position and return
	li $t6, -2
	li $t7, -2
	j loop_draw_centered_square
	
loop_draw_centered_square:
	and $t5, $t6, $t7
	beq $t5, 2, done_return_nested # if both iterators are = 2
	
	add $a1, $t3, $t6
	add $a2, $t4, $t7
	addi $t6, $t6, 1
	jal draw_pixel
	beq $t6, 2, square_increment_height
	j loop_draw_centered_square
	
square_increment_height:
	li $t6, -2
	addi $t7, $t7, 1
	j loop_draw_centered_square
	
######################################
# Draws a centered x at current x,y with a size of 9 x 9.

# Should be called with a jal

# Set before calling:
# t3 - incoming x to save
# t4 - incoming y to save

# Used in function:
# t6 = top iterator (-6 to 6)
# t7 = side iterator (-6 to 6)
# t5 = used in arithmetic

draw_centered_x: 
	save_pos_ra	# save position and return
	li $t6, -6
	li $t7, -6
	j loop_draw_centered_x
	
loop_draw_centered_x:
	and $t5, $t6, $t7
	beq $t5, 7, x_draw_other # if both iterators are = 7
	add $a1, $t3, $t6
	add $a2, $t4, $t7
	addi $t6, $t6, 1
	addi $t7, $t7, 1
	jal draw_pixel
	j loop_draw_centered_x
	
x_draw_other:
	li $t6, 6
	li $t7, -6
	j loop_draw_other_x
	
loop_draw_other_x:
	beq $t7, 7, done_return_nested # if side iterator is = 7
	add $a1, $t3, $t6
	add $a2, $t4, $t7
	addi $t6, $t6, -1
	addi $t7, $t7, 1
	jal draw_pixel
	j loop_draw_other_x
	
######################################
	
draw_centered_o:
	save_pos_ra
	
	# start at upper left-hand corner
	li $t5, -7
	li $t6, -7
	
	# move to the right
	li $t7, 1
	li $t8, 0
	li $t0, 0
	jal loop_centered_o
	
	# move down
	li $t7, 0
	li $t8, 1
	li $t0, 0
	jal loop_centered_o
	
	# move to the left
	li $t7, -1
	li $t8, 0
	li $t0, 0
	jal loop_centered_o
	
	# move up
	li $t7, 0
	li $t8, -1
	li $t0, 0
	jal loop_centered_o
	j done_return_nested
	
# call with intended starting x,y in $t5, $t6
# call with intended x-direction, y-direction in $t7, $t8
loop_centered_o:
	save_ra
	beq $t0, 14, return_to_o_loop # if length indicator is = 14
	add $a1, $t3, $t5
	add $a2, $t4, $t6
	add $t5, $t5, $t7
	add $t6, $t6, $t8
	addi $t0, $t0, 1
	jal draw_pixel
	restore_ra
	j loop_centered_o
	
return_to_o_loop:
	restore_ra
	jr $ra
	
######################################

clear_board:
	save_pos_ra	# save position and return
	li $t0, 0
	li $t1, WIDTH
	li $t2, 0
	li $t3, HEIGHT
	li $a3, BLACK
	j clear_board_loop

clear_board_loop:
	add $t4, $t0, $t2
	add $t5, $t1, $t3
	beq $t4, $t5, done_return_nested
	move $a1, $t0
	move $a2, $t2
	jal draw_pixel_no_delay
	addi $t0, $t0, 1
	beq $t0, $t1, clear_increment_height
	j clear_board_loop
	
clear_increment_height:
	li $t0, 0
	addi $t2, $t2, 1
	j clear_board_loop
	
######################################

done_return_nested:
	restore_pos_ra
	jr $ra

######################################

.macro pixel_delay
	li $v0, 32
	li $a0, 1
	syscall
.end_macro

.macro draw_me
	# s1 = address = $gp + 4*(x + y*width)
	mul	$t9, $a2, WIDTH   # y * WIDTH
	add	$t9, $t9, $a1  # add X
	mul	$t9, $t9, 4	  # multiply by 4 to get word offset
	addi	$t9, $t9, MEM	  # add to base address
	sw	$a3, ($t9)	  # store color at memory location
.end_macro

######################
# draw a pixel
# $a1 = x
# $a2 = y
# $a3 = color	
draw_pixel:
	# s1 is the address of the pixel to update
	# s1 = (0,0) + 4*(x + y*width)
	draw_me
	pixel_delay
	jr $ra
	
draw_pixel_no_delay:
	draw_me
	jr $ra

######################################

exit: # Terminate the program. Should only be called if an ESC is entered in check_in, or the game has ended.
	li $v0, 10
	syscall
