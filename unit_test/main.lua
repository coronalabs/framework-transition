-------------------------------------------------------------------------------
-- Code is MIT licensed; see https://www.coronalabs.com/links/code/license
-- File: main.lua
-- 
-- Change the package.path and make it so we can require the "timer.lua" file from the root directory
-------------------------------------------------------------------------------

local path = package.path

-- get index of first semicolon
local i = string.find( path, ';', 1, true )
if ( i > 0 ) then
	-- first path (before semicolon) is project dir
	local projDir = string.sub( path, 1, i )

	-- assume dir is parent to projDir
	local dir = string.gsub( projDir, '(.*)/([^/]?/\?\.lua)', '%1/../%2' )
	package.path = dir .. path
end

-- Nil out anything loaded from the core so we use the local versions of the files.
transition = nil
package.loaded.transition = nil
package.preload.transition = nil

-------------------------------------------------------------------------------

local W = display.contentWidth
local H = display.contentHeight

local isPause = false
local transitionNew = require("transition")

local w = 0.5*W
local h = 0.25*H
local r1 = display.newRect(1,1, w, h)
r1:setFillColor(255,0,0)
local executions = 0

local delay = 0
local function onComplete( self )
	self.alpha = 1

	self.t = nil
	local t = transitionNew.to( self,
		{ alpha = 0, time = 3000, delay = delay, transition = easing.inOutQuad, onComplete=self} )

	self.t = t
	delay = delay + 10
end

local r2 = display.newRect( w, h, w, h )
r2:setFillColor( 0, 255, 0)
r1.onComplete = onComplete
r2.onComplete = onComplete

local r3 = display.newRect( 1, 2*h, w, h )
r3:setFillColor( 0, 0, 255 )
transitionNew.blink( r3, { time = 3000 } )

local r4 = display.newRect( 2 * w, 2 * h, w, h )
r4:setFillColor( 0, 0, 255 )
transitionNew.blinkContinuous( r4, { time = 3000 } )

function touchHandler (event)
	if (event.phase == "began") then
		transitionNew.cancel( event.target.t )
		print( "Rect cancelled!")
	end
end

function touchHandlerPause (event)
	if (event.phase == "began") then
		if false == isPause then
		transitionNew.pause( event.target.t )
		print( "Rect paused!" )
		isPause = true
		elseif true == isPause then
		transitionNew.resume ( event.target.t )
		print( "Rect resumed!" )
		isPause = false
		end
	end
end

r1:addEventListener("touch", touchHandler)
r2:addEventListener("touch", touchHandlerPause)

-- Starts the repeating transition. Each time the transition completes, 
-- it restarts with an increasing delay
r1:onComplete()
r2:onComplete()


local arr_r = {}
local isArrPause = false
local pauseKey = "p"
local count = 100
for i = 1, count do
	local r_i = display.newRect(W/(count+1) * i, H, W/(count+1), 20)
	r_i.anchorY = 1

	-- example of shifted transition with delay
	r_i.i = i
	transitionNew.scaleTo(r_i, {yScale = 10, time = 10000, timeShift = i * 10000/count, transition = easing.outInBounce, delay = 3000, onPause = function(obj) print("paused " .. obj.i) end})
	
	r_i:setFillColor(0, (i%2 == 1 and 0 or 1), (i%2 == 0 and 0 or 1))
	arr_r[#arr_r+1] = r_i
end

function keuHandlePause(event)
	if event.keyName == pauseKey and event.phase == "down" then
		if false == isArrPause then
			for i = 1, count do
				transitionNew.pause(arr_r[i])
			end
			print( "Array paused!" )
			isArrPause = true
		elseif true == isArrPause then
			for i = 1, count do
				transitionNew.resume(arr_r[i])
			end
			print( "Array resumed!" )
			isArrPause = false
		end
	end
end


Runtime:addEventListener("key", keuHandlePause)