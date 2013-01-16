-------------------------------------------------------------------------------
-- Copyright (C) 2012 Corona Inc. All Rights Reserved.
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

local isPause = false
local transitionNew = require("transition")

local w = 0.5*display.contentWidth
local h = 0.5*display.contentHeight
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

function touchHandler (event)
	if (event.phase == "began") then
		transitionNew.cancel( event.target.t )
		print( "Cancelled!")
	end
end

r1:addEventListener("touch", touchHandler)
r2:addEventListener("touch", touchHandler)

-- Starts the repeating transition. Each time the transition completes, 
-- it restarts with an increasing delay
r1:onComplete()
r2:onComplete()