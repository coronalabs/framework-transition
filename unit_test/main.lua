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

local buttonrect = display.newRect(1,1,display.contentWidth,display.contentHeight)
buttonrect:setFillColor(255,0,0)
local executions = 0
local t1 = transitionNew.to( buttonrect, { alpha = 0.2, time = 3000, transition = easing.inOutQuad})

function touchHandler (event)
	if (event.phase == "began") then
		transitionNew.cancel( t1 )
		print( "Cancelled!")
	end
end


buttonrect:addEventListener("touch", touchHandler)