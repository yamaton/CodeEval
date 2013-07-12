{-
robot_movements.hs

Description
------------
A robot is located at the top-left corner of a 4x4 grid. The robot can move either up, down, left, or right, but can not visit the same spot twice. The robot is trying to reach the bottom-right corner of the grid.

Input sample
-------------
There is no input for this program.

Output sample
--------------
Print out the unique number of ways the robot can reach its destination. (The number should be printed as an integer whole number eg. if the answer is 10 (its not !!), print out 10, not 10.0 or 10.00 etc)


-}

def robot_movements(grid, start, goal):
    result = []
    stack = [ [start] ]
    delta = [[0, 1], [1, 0], [0, -1], [-1, 0]]
    maxX, maxY = len(grid), len(grid[0])
    
    while stack:
        # standard breath-fast search
        path = stack.pop(0)
        p = path[-1]
        for direction in delta:
            nextX = p[0] + direction[0]
            nextY = p[1] + direction[1]
            if (0 <= nextX < maxX and 0 <= nextY < maxY 
                    and (nextX, nextY) not in path):
                newpath = path[:]
                newpath.append((nextX, nextY))
                
                if (nextX, nextY) == goal:
                    result.append(newpath)
                else:
                    stack.append(newpath)
    return len(result)


def test():
    grid = [[0, 1],
            [2, 3]]
    assert robot_movements(grid, (0,0), (1,1)) == 2
    assert robot_movements(grid, (0,0), (0,1)) == 2
    
    grid = [[0, 1, 2],
            [3, 4, 5]]
    assert robot_movements(grid, (0,0), (1,1)) == 3
    assert robot_movements(grid, (0,0), (1,0)) == 3
    print "passed all tests!"



if __name__ == '__main__':
    x = range(16)
    grid = [x[4*i:4*i+4] for i in range(4)]
    print robot_movements(grid, (0,0), (3,3))
