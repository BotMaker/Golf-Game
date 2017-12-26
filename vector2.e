global constant TRUE=1
global constant FALSE=0

global constant BALL_WHITE=1
global constant BALL_BLACK=2

global constant BALL_YELLOW1=3
global constant BALL_YELLOW2=4
global constant BALL_YELLOW3=5
global constant BALL_YELLOW4=6
global constant BALL_YELLOW5=7
global constant BALL_YELLOW6=8
global constant BALL_YELLOW7=9

global constant BALL_RED1=10
global constant BALL_RED2=11
global constant BALL_RED3=12
global constant BALL_RED4=13
global constant BALL_RED5=14
global constant BALL_RED6=15
global constant BALL_RED7=16

-- one of these is return by the white ball
global constant NOBALL=0
global constant REDBALL=1
global constant YELLOWBALL=2
global constant BLACKBALL=3
global constant WHITEBALL=4 --this is not used by a.i

global sequence white_ball_first_hit
global integer white_ball_check_hit
white_ball_first_hit={0,NOBALL}
white_ball_check_hit=TRUE




without warning
without type_check


-------------------------------------------------------------------------------
--Constants
-------------------------------------------------------------------------------
global constant         --Index Labels.
         POS = 1, RAD = 2, VEL = 3, BOUNCE = 4,           --Ball object
         APT = 1, BPT = 2 -- VEL = 3, BOUNCE = 4          --Line object
global constant X = 1
global constant Y = 2

global constant PIo2 = 1.570796327
constant          PI = 3.141592654
constant         PI2 = 6.283185307


-------------------------------------------------------------------------------
--Types
-------------------------------------------------------------------------------
--Basic vector datatype. of the form {xv, yv}
global type vect(object s)
    if sequence(s) and length(s) = 2 then
        return 1
    else return 0
    end if
end type

--Basic coordinate type. of the form {xpos, ypos}
global type coord(object s)
    if sequence(s) and length(s) = 2 then
        return 1
    else return 0
    end if
end type

--Circle. of the form { coord , radius }
global type circle(sequence s)
    return length(s) = 2 and coord(s[1]) and atom(s[2])
end type


--OBJECT Types
--============

global constant OBJ_LEN = 5 --If you with to give lines or balls additional attributes, you
                            --can add this offset to your index constants to ensure compatibility
                            --with subsequent versions of vector.e

--Line. Of the form { APT, BPT, VEL, BOUNCE, etc}
global type line(sequence s)
    if length(s) >= OBJ_LEN-1 and coord(s[1]) and coord(s[2])
                            and vect(s[3]) and atom(s[4]) then
        return 1
    else
        return 0
    end if
end type

--Ball. Of the form { POS , RAD , VEL, BOUNCE, etc.... }
global type ball(sequence s)
    if length(s) >= OBJ_LEN and coord(s[1]) and atom(s[2]) and 
                       coord(s[3]) and atom(s[4]) and sequence(s[5]) then
        return 1
    else
        return 0
    end if
end type



-------------------------------------------------------------------------------
--Object Creation
-------------------------------------------------------------------------------
--Create a new Ball object, with parameters of that given
global function newBall(sequence pos, atom rad, sequence vel, atom bounce,sequence data)
    return {pos,rad,vel,bounce,data} 
end function

--Create a new Line object, with parameters of that given
global function newLine(sequence apt, sequence bpt, sequence vel, atom bounce)
    return {apt,bpt,vel,bounce}
end function

--Create a series of lines from a coord-sequence.
--Place points in between each line (corners)
--Velocity is the same overrall.
global function newLines(sequence lines, sequence vel, atom bounce,sequence data)
    sequence O
    if not length(lines) then return {} end if
    O = {{lines[1],0,vel,bounce,{}}}
    for a = 1 to length(lines)-1 do
        O &= {{lines[a],lines[a+1],vel,bounce,{}},
              {lines[a+1],0,vel,bounce,{}}}
    end for
    return O
end function

-------------------------------------------------------------------------------
--Utility Functions
-------------------------------------------------------------------------------

--Version of floor that compensates for negative numbers
global function nfloor(object t)
    return floor(t) + (t < 0)
end function

--If |t| is less than 0.0001, return 0, else t
global function cut(object t)
    return t*(t > 0.0001) + t*(t < 0.0001)
end function

--Sgn returns sign(s) of an object.
global function sgn(object s)
    return 1 - 2*(s < 0)
end function

--Return absolute value.
function abs(object s)
    return s *( 1 - 2*(s < 0) )
end function

--Return the higher object
global function max(object a, object b)
    if compare(a,b) > 0 then
        return a
    else return b
    end if
end function

--Return the lower object
global function min(object a, object b)
    if compare(a,b) < 0 then
        return a
    else return b
    end if
end function

--returns |s|^2
global function distv_sq(vect s)
    return s[X]*s[X] + s[Y]*s[Y]
end function

--returns the square of the distance between two points
global function dist_sq(coord a, coord b)
    return distv_sq(a - b)
end function

--returns magnitude of vector. (square root of |s|^2 )
global function distv(vect s)
    return sqrt(s[X]*s[X] + s[Y]*s[Y])
end function

--returns the angle of the vector s, in radians
global function angle_v(vect s)
    atom a 
    if not s[X] then
        return 1.570796327 * (1 - 2*(s[Y] < 0))
    elsif not s[Y] then
        return PI * (s[X] < 0)
    end if
    a = arctan(s[Y]/s[X])
    if s[X] < 0 then        --If the |angle| is > PIo2
        if s[Y] > 0     then return a + PI
        elsif s[Y] < 0  then return a - PI
        end if
    end if
    return a
end function

--returns the angle of the line from a to b
global function angle_p(coord a, coord b)
    return angle_v(b - a)
end function

--Wrap an angle around to -PI < angle < PI
global function anglise(atom a)
    while a > PI do a -= PI2 end while
    while a < -PI do a += PI2 end while     
    return a
end function

--returns a vector of length 1, with the same angle as V.
global function normalise(vect V)
    return V/sqrt(V[X]*V[X] + V[Y]*V[Y])
end function

--Return the dot product of the two vectors passed to it; a and b.
global function dot(vect a, vect b)
    return a[X]*b[X]+a[Y]*b[Y]
end function

--returns the square of the perpendicular distance from point to line.
global function disttoline_sq(coord p, line l)
    atom a2, b2, c2
    a2 = power(p[X] - l[APT][X], 2) + power(p[Y] - l[APT][Y], 2)
    if equal(l[APT],l[BPT]) then
        return  a2 
    end if
    b2 = power(p[X] - l[BPT][X], 2) + power(p[Y] - l[BPT][Y], 2)
    c2 = power(l[BPT][X] - l[APT][X], 2) + power(l[BPT][Y] - l[APT][Y], 2)    
    return (4*b2*c2 - power(b2+c2-a2,2)) / (4*c2)
end function


-------------------------------------------------------------------------------
---Collision Detection functions
-------------------------------------------------------------------------------

--Finds the collision time between two ball objects.
--Returns -1, or the fraction of dT that it takes to hit the obstacle
global function collision_time_ballball(ball BallA, ball BallB)
    vect F
    atom A,B,C                  --Components of the quadratic
    atom D                      --Determinant of the quadratic
    atom RS                     --Combined radii of ballA and ballB
    atom tc                     --Fraction of frame in which ballA collides with ballB
    vect W                      --Relative velocity

    --If BallB is moving, then the velocity of the two must be combined.
    W = BallA[VEL] - BallB[VEL]
    
    --If the two ball's relative velocity is zero, then there is no way they can collide.
    if equal(W,{0,0}) then return -1 end if

    F = BallA[POS] - BallB[POS] --F is vector difference between the two circles (initial)
    RS = BallA[RAD] + BallB[RAD] --Combined radii of ballA and ballB
    
    --Quadratic equation simplifies down to At^2 + 2Bt + C = 0, where:
    A = W[X]*W[X] + W[Y]*W[Y]
    B = W[X]*F[X] + W[Y]*F[Y]
    C = F[X]*F[X] + F[Y]*F[Y] - RS*RS
    --Determinant is B^2 - A*C
    D = B*B - A*C   

    --If determinant is less than zero, then no collision
    --If determinant is zero, then only a graze (no collision)
    if D <= 0 then 
        return -1
    --If determinant is > 0 then there might be a collision 
    else
        tc = -(B+sqrt(D))/A --The fraction of the deltaT that BallA will hit BallB with.
                            --If less than 0, then not hit. If more than 1, then not hit.
        --If the ball will hit the obstacle this frame
        if tc >= 0 and tc < 1 then      return tc
        else                            return -1 --no collision
        end if
    end if
end function

-------------------------------------------------------------------------------
--Finds the collision time between a moving ball and a stationary line.
--Returns {-1,{0,0}}, or
--{the fraction of dT that it takes to hit the obstacle, the point where it hit}
global function collision_time_ballline(ball BallQ, line L)
    sequence S                  --S is the difference between the two points (A-B)
    sequence W                  --W is the velocity (displacement over dT)
    sequence F                  --F is a vector from Q to B
    atom kc,tc                  --kc is the fraction of the distance between A and B where the collision occurs.
                                --tc is the fraction of dT that passes before a collision occurs.
    atom g                      --g is 1/(S.S): A commonly used value in the quadratic components
    atom SdotW,SdotF            --More commonly used values
    atom qA,qB,qC               --Components of the quadratic equation used for the solution.
                                --prefaced with q to avoid confusion with the line.
    atom D                      --The determinant of the quadratic.
                                

    W = BallQ[VEL] - L[VEL]     --W is the initial velocity

    --If the relative velocity is zero, then there is no way they can collide.
    if equal(W, {0,0}) then return {-1,{0,0}} end if
    
    S = L[APT] - L[BPT]         --S is the difference between the two points	
    F = BallQ[POS] - L[BPT]     --F is the vector from BallQ[POS] to B

    g = 1/dot(S,S)              --Optimisations
    SdotW = dot(S,W)
    SdotF = dot(S,F)

    qA = dot(W,W) - g*SdotW*SdotW    --Final equations of the quadratic components
    qB = dot(W,F) - g*SdotW*SdotF
    qC = dot(F,F) - g*SdotF*SdotF - BallQ[RAD]*BallQ[RAD]

    D = qB*qB - qA*qC               --Determinant of the quadratic above
    
    --If the determinant or the first component is negative, then no collision
    if qA = 0 then return {-1,{0,0}}
    elsif D <= 0 then
        return {-1,{0,0}}
    --If the determinant is positive, evaluate tc and kc
    else
        tc = -(qB + sqrt(D))/qA             --tc is time to collision
        kc = g*(tc*SdotW + SdotF)       --kc is fraction of collision
        --If both of these are between 0 and 1, then a collision occured
        if tc >= 0 and tc < 1 and kc > 0 and kc < 1 then 
                return {tc,S*kc + L[BPT]} --Return tc, and the place it collided at
        else    return {-1,{0,0}} --No collision
        end if
    end if
end function


-------------------------------------------------------------------------------
--Motion Functions
-------------------------------------------------------------------------------
--Bounce one object off another. Weights are given as mass1 and mass2.
--If one object should be unaffected by the collision, make its mass 1 and the other's mass 0
global function objectBounce(vect W1, vect V1, atom mass1, atom H1,
                             vect W2, vect V2, atom mass2, atom H2,
                             vect N)
    --W*			--Initial displacement per relative frame (velocity for this subframe)
    --V*			--Initial real velocity
    --mass*			--Mass of the object
    --H*			--Bounce
    --N				--Normal to the collision 
                    --(vector passing through one object and the collision point)
    atom H          --Averaged bounce.
    sequence Wr, Vr     --Relative W and V to each other
    atom c              --optimised... H/(mass1+mass2)
    atom k              --optimised #2... c*(a1-a2)
    sequence Wi1, Wi2   --New W after collision
    sequence Vi1, Vi2   --new V after collision
    --Wa1,Wa2			--components of W that are parallel to N
    --Va1,Va2			--components of V that are parallel to N
    

    Wr = W1 - W2        --Relative W
    Vr = V1 - V2        --Relative V
    
    N = normalise(N)    --Reduce normal to a normalised vector.

    --Calculate bounce
    H = 1.03 + 0.5*(H1+H2)
    if H > 2 then H = 2 end if --Prevents violation of laws of thermodynamics (read: Chaos. Trust me.)
    
    c = H/(mass1+mass2)     --optimisation - only calculated once
    --Calculating Wi1 and Wi2
    k = (N[X]*Wr[X]+N[Y]*Wr[Y])*c
    Wi1 = W1 - k*mass2*N
    Wi2 = W2 + k*mass1*N

    --Calculating Vi1 and Vi2
    k = (N[X]*Vr[X]+N[Y]*Vr[Y])*c
    Vi1 = V1 - k*mass2*N
    Vi2 = V2 + k*mass1*N

    return {Wi1, Vi1, Wi2, Vi2}
end function


-------------------------------------------------------------------------------
--Move balls in the scene, with realistic collisions off each other and the objects in the scene
--Returns the Balls sequence with updated values.
--
--Obstacles may be either balls or lines, and may be static or moving. However, obstacles do not
--bounce off each other, and are unaffected by the balls themselves, though the balls will bounce off them
global integer trace_flag trace_flag = 0
global function moveBalls(sequence Balls, sequence Obstacles, sequence gravity, atom air_friction)
    atom this_tc, min_tc    --The fraction of the frame (or subframe) in which a collision occurs
    atom min_a, min_b       --The respective indices of the two objects that collided
                            --min_a will only ever be a Ball, min_b may be either a ball or an obstacle
    sequence this_N,min_N   --The normal of the collision.
                            
    sequence realVel        --REAL velocities of the Objects. (not subframe vels)

    integer ballsNum        --The number of balls in the scene
    integer totalObj        --Balls + Obstacles

    object temp

    sequence Object     --A master sequence that contains both Balls and Obstacles

    ballsNum = length(Balls)
    totalObj = ballsNum + length(Obstacles)
    
    --Create a homogenous sequence of objects. Length is totalObj, 
    --and up to Objects[ballsNum] is all Balls.
    Object = Balls & Obstacles
    
    --Store the original velocities of the Objects in realVel
    realVel = {}
    for a = 1 to totalObj do
        realVel &= {Object[a][VEL]}
    end for 

    --Repeat until no more collisions occur
    while 1 do
        if trace_flag then trace(1) end if
        min_a = 0 --Balls that collide first
        min_b = 0
        min_tc = 999 --Fraction of frame until first collision

        --For every combination of Balls, and every combination of a Ball and an Obstacle...
        for a = 1 to ballsNum do        --(these index limits for a and b ensure that all
            for b = a+1 to totalObj do  -- balls collide with each other, and no obstacles
                                        -- try to collide with each other.)
                --Check to see if ball a collides with object b
                if line(Object[b]) then --If b is a Line ('a' will always be a ball)
                    temp = collision_time_ballline(Object[a], Object[b])
                    this_tc = temp[1]
                    this_N  = temp[2]
                else                    --If b is a Balls ball, or an obstacle ball.
                    this_tc = collision_time_ballball(Object[a], Object[b])
                    this_N  = Object[b][POS]
                end if
                
                --If there is a collision, and it occurs before the last found collision
                if this_tc != -1 and this_tc < min_tc then
                    min_tc = this_tc    --Time of collision
                    min_a = a           --First object that collided
                    min_b = b           --Second object that collided
                    min_N = this_N
                end if
            end for
        end for
        
        --If there was a collision...
        if min_a then
            min_tc -= 0.0000002     --FUDGE Factor
                                    --Rewinds time a little to prevent endless collisions			

            --Calculate the normal
            min_N = min_N - Object[min_a][POS] + min_tc*(Object[min_b][VEL]  - Object[min_a][VEL])      

            --Move all the Objects to the new position in the frame
            for i = 1 to totalObj do
                Object[i][POS] += min_tc*Object[i][VEL] --Move Object[a][POS] to the collision position
                if i > ballsNum and line(Object[i]) then --If Object is a line, we have to add to both endpoints.
                    Object[i][BPT] += min_tc*Object[i][VEL]
                end if
                Object[i][VEL] *= (1-min_tc)      --Reduce Object[a][VEL] to the remainder of the frame's v	
            end for

            --this checks to see what ball the white ball hits first
            if white_ball_check_hit=TRUE and not line(Object[min_a]) and not line(Object[min_b]) then
             if length(Object[min_a][5]) and length(Object[min_b][5]) then --fix a bug 
             
              if Object[min_a][5][1]=BALL_WHITE then --white ball
                white_ball_first_hit={Object[min_b][5][1],Object[min_b][5][2]}
                white_ball_check_hit=FALSE
              elsif Object[min_b][5][1]=BALL_WHITE then
                white_ball_first_hit={Object[min_a][5][1],Object[min_a][5][2]}
                white_ball_check_hit=FALSE
              end if

             end if
            end if

            --Bounce the objects off each other
            -- -1 is interpreted as infinite mass
            temp = objectBounce(
                    Object[min_a][VEL], realVel[min_a], (min_b<=ballsNum),Object[min_a][BOUNCE],
                    Object[min_b][VEL], realVel[min_b], 1,Object[min_b][BOUNCE],
                    min_N   )
            
            --temp is in the form {Wi1, Vi1, Wi2, Vi2}
            Object[min_a][VEL]  = temp[1]
            realVel[min_a]      = temp[2]
            Object[min_b][VEL]  = temp[3] --Don't worry, if the object was an obstacle, 
            realVel[min_b]      = temp[4] --temp[3] and temp[4] will not change
                                        

            for i = 1 to ballsNum do --For every Ball (obstacles are not affected by gravity or air friction)
                Object[i][VEL] += min_tc*gravity    --Add part of gravity to the frame velocity
                realVel[i]     += min_tc*gravity    --And the real velocity
                Object[i][VEL] *= 1-min_tc*air_friction --Reduce velocity by 
                realVel[i]     *= 1-min_tc*air_friction --a factor of 1-air_friction
            end for 
            gravity *= (1-min_tc)   --Cut gravity back to what remains of the frame
            air_friction *= (1-min_tc) --and air friction as well.
            
        --If no collisions left 
        else
            if equal(gravity, {0,0}) and air_friction = 0 then 
                for i = 1 to ballsNum do
                    Object[i][POS] += Object[i][VEL]    --Add the remainder of the frame's velocity to the last position
                    Object[i][VEL] = realVel[i]     --Set velocity back to the real value 
                end for
                return Object[1..ballsNum] --Return all the balls
            else
                for i = 1 to ballsNum do
                    Object[i][VEL] += gravity --Add whatever gravity remains to the frame velocity
                    realVel[i]     += gravity --and the real velocity
                    Object[i][VEL] *= 1-air_friction --Reduce velocity by 
                    realVel[i]     *= 1-air_friction --a factor of 1-air_friction
                end for 
                gravity = {0,0} --Clear gravity : all its force has been used.
                air_friction = 0

                --Return through the objects one more time to check that the additional gravity
                --and the reduced velocity didn't cause any more collisions (VERY IMPORTANT)
            end if
        end if
    end while
end function