
all : compile;

% : sbt_%;

sbt_%:
	sbtn --batch $*



