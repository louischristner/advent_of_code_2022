NAME1	=	solver1
NAME2 = solver2

SRC1	=	Day03_part1.hs
SRC2	=	Day03_part2.hs

SRC_LIB	=	Utils.hs

all: part1 part2

part1: $(NAME1)

part2: $(NAME2)

$(NAME1):
	ghc -o $(NAME1) $(SRC1) $(SRC_LIB)

$(NAME2):
	ghc -o $(NAME2) $(SRC2) $(SRC_LIB)