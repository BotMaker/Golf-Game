include vector2.e
include get.e

constant M_DRLINE = 2,
         M_DRLINES= 3,
         M_DRCIRC = 4,
         M_DRBALL = 5

-------------------------------------------------------------------------------
global function load_data(sequence filename)
    atom fn
    sequence data
    
    fn = open(filename, "r")
    if fn = -1 then return -1 end if

    data = get(fn)

    if data[1] != GET_SUCCESS then return -1 end if

    return data[2]
end function

-------------------------------------------------------------------------------
global function save_data(sequence filename, sequence data)
    atom fn

    fn = open(filename, "w")

    if fn = -1 then return -1 end if

    print(fn, data)
    close(fn)
    return 0
end function

-------------------------------------------------------------------------------
global function create_objects(sequence data, integer cornersflag, atom bounce)
    sequence balls, obstacles
    sequence cornerslist
    balls = {}
    obstacles = {}
    cornerslist = {}

    for a = 1 to length(data) do
        if data[a][1] = M_DRLINE then
            obstacles &= {newLine(data[a][2],data[a][3],{0,0},bounce)}
            if cornersflag then
                if not find(data[a][2], cornerslist) then obstacles &= {newBall(data[a][2],0,{0,0},bounce,{})} end if
                if not find(data[a][3], cornerslist) then obstacles &= {newBall(data[a][3],0,{0,0},bounce,{})} end if
            end if
        elsif data[a][1] = M_DRLINES then
            if cornersflag and not find(data[a][2], cornerslist) then 
                obstacles &= {newBall(data[a][2],0,{0,0},bounce,{})} 
            end if
            for b = 2 to length(data[a]) - 1 do
                obstacles &= {newLine(data[a][b],data[a][b+1],{0,0},bounce)}
                if cornersflag and not find(data[a][b+1], cornerslist) then 
                    obstacles &= {newBall(data[a][2],0,{0,0},bounce,{})} 
                end if
            end for
        elsif data[a][1] = M_DRCIRC then
            obstacles &= {newBall(data[a][2],distv(data[a][2]-data[a][3]),{0,0},bounce,{})}
        elsif data[a][1] = M_DRBALL then
            balls &= {newBall(data[a][2],distv(data[a][2]-data[a][3]),{0,0},bounce,{})}
        end if
    end for
    return {balls, obstacles}
end function
-------------------------------------------------------------------------------	