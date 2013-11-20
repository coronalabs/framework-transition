-------------------------------------------------------------------------------
--
-- transition.lua
--
-- Copyright (C) 2013 Corona Labs Inc. All Rights Reserved.
--
-------------------------------------------------------------------------------
print("v1")
-- copies all key/value pairs
local function copyTable( src )
	local t = {}

	for k,v in pairs( src ) do
		t[k] = v
	end

	return t
end

-------------------------------------------------------------------------------

-- NOTE: transition is assigned to the global var "transition" in init.lua.
-- This file should follos standard Lua module conventions
local transition = { _activeTweens = {} }

transition._nonPropertyKeys =
{
	time=true, transition=true, delay=true, delta=true,
	onStart=true, onComplete=true
}

local numberKeys =
{
	x=true, y=true, xScale=true, yScale=true, rotation=true, width=true, height=true, 
	alpha=true, 
	xReference=true, yReference=true, 
	maskX=true, maskY=true, maskScaleX=true, maskScaleY=true, maskRotation=true,
	delta=true,
}

local function validateValue( k, v )
	if ( "number" == type( v ) and numberKeys[k] ) then
		local isNan = ( v ~= v )
		if ( isNan ) then
			print( "ERROR: Invalid number argument provided. Attempt to set property " .. k .. " to NaN." )
		end
		assert( not isNan )
	end

	return v
end

transition._initTween = function( tween, parameters )
	local target = tween._target

	-- onStart listener
	--[[
	local onStart = tween._onStart
	if onStart and type(onStart) == "function" then
		onStart( target )
		tween._onStart = nil -- only call once
	end
	--]]
	local listener = tween._onStart
	if listener then
		Runtime.callListener( listener, "onStart", target )
		tween._onStart = nil -- only call once
	end


	local keysStart = {}
	local keysFinish = {}
	local invalidKeys = transition._nonPropertyKeys
	local isDeltaValue = validateValue( "delta", parameters.delta )
	for k,v in pairs( parameters ) do
		if not invalidKeys[k] then
			-- Only tween properties that have a non-nil starting value
			local startValue = target[k]
			if startValue then
				keysStart[k] = startValue

				local finishValue = (isDeltaValue and (startValue+v)) or v
				keysFinish[k] = validateValue( k, finishValue )
--print( "Tween("..k..") ["..startValue..","..keysFinish[k].."]" )
			end
		end
	end
	tween._keysStart = keysStart
	tween._keysFinish = keysFinish
end

transition._add = function( tween )
	local activeTweens = transition._activeTweens

	-- Once we have at least one tween, register for frame events
	if #activeTweens == 0 and not transition._hasEventListener then
		transition._hasEventListener = true
		Runtime:addEventListener( "enterFrame", transition )
	end

	table.insert( activeTweens, tween )
end

transition.to = function( target, parameters )
	local tween = nil

	if target and parameters then
		-- faster to access a local timer var than a global one
		local transition = transition

		tween = {}
		local t = system.getTimer()
		tween._target = target

		tween._timeStart = t
		tween._duration = parameters.time or 500
		tween._transition = parameters.transition or easing.linear
		tween._onStart = Runtime.verifyListener( parameters.onStart, "onStart" )
		tween._onComplete = Runtime.verifyListener( parameters.onComplete, "onComplete" )

		local delay = parameters.delay
		if type(delay) == "number" then
			tween._delay = delay

			-- save off params: init after delay to minimize race conditions
			local params = copyTable( parameters )
			tween._delayParams = params
--[[
			local params = {}
			tween._delayParams = params
			for k,v in pairs( parameters ) do
				params[k] = v
			end
--]]
		else
			-- no delay, so init immediately
			transition._initTween( tween, parameters )
		end

		transition._add( tween )
	end

	return tween
end

transition.from = function( target, parameters )
	local tween = nil

	if target and parameters then
		local params = {}
		local invalidKeys = transition._nonPropertyKeys
		for k,v in pairs( parameters ) do
			if not invalidKeys[k] then
				params[k] = target[k]
				target[k] = validateValue( k, v )
			else
				-- copy over non-property keys, e.g. time, delay, etc.
				params[k] = v
			end
		end

		tween = transition.to( target, params )
	end

	return tween
end

transition.cancel = function( tween )
	tween._cancel = true
end

function transition:enterFrame( event )
	-- Calls to listeners could trigger a call to transition:add,
	-- causing changes to the tween array during iteration of that array.
	-- To prevent contention, we swap it with a tmp tween table and then later
	-- restore the original, appending any new tweens added to the tmp table
	local currentActiveTweens = self._activeTweens
	self._activeTweens = {}

	local currentTime = event.time

	-- TODO: See if we can remove during traversal
	local toRemove = {}

--print( "Num active tweens: ".. #currentActiveTweens )

	for i,tween in ipairs( currentActiveTweens ) do
--print( "["..i.."]" )
		if tween._cancel then
			-- queue up cancelled tween for removal
			table.insert( toRemove, i )
		else
			local delay = tween._delay
			if delay and ( currentTime >= (tween._timeStart + delay) ) then
				tween._delay = nil
				tween._timeStart = currentTime
				delay = nil
			end

			if not delay then
				local params = tween._delayParams
				if params then
					transition._initTween( tween, params )
					tween._delayParams = nil
				end

				local target = tween._target
				local keysFinish = tween._keysFinish
				local t = currentTime - tween._timeStart
				if t < 0 then t = 0 end
				local tMax = tween._duration
				if t < tMax then
					for k,v in pairs( tween._keysStart ) do
						target[k] = tween._transition( t, tMax, v, keysFinish[k] - v )
	--local newVal = tween._transition( t, tMax, v, keysFinish[k] - v )
	--print( "\t("..t.."/"..tMax..") "..tostring(target) .."["..k.."] = "..target[k].."(=="..newVal..")" )
					end
				else
					for k,v in pairs( keysFinish ) do
						target[k] = v
	--print( "\t("..t..">="..tMax..") "..tostring(target) .."["..k.."] = "..v )
					end

					-- onComplete listener
					local listener = tween._onComplete
					if listener then
						Runtime.callListener( listener, "onComplete", target )
					end

					-- queue up complete tween for removal
					table.insert( toRemove, i )
				end
			end
		end
	end

	for i=#toRemove,1,-1 do
		table.remove( currentActiveTweens, toRemove[i] )
	end

	-- Swap out tmp tween table and restore self._activeTweens.
	-- Append any new tweens in the tmp table back into _activeTweens
	local tmpTweens = self._activeTweens
	if #tmpTweens > 0 then
		for _,tween in ipairs( tmpTweens ) do
			table.insert( currentActiveTweens, tween )
		end
	end
	self._activeTweens = currentActiveTweens

	if #currentActiveTweens == 0 then
--print( "remove transition.enterFrame" )
		Runtime:removeEventListener( "enterFrame", transition )
		transition._hasEventListener = false
	end
end

function transition._setInvisible( object )
	object.isVisible = false
end

function transition._dissolvePrepareSrc( object )
	object.alpha = 1
end

function transition._dissolvePrepareDst( object )
	object.alpha = 0
	object.isVisible = true
end

function transition.dissolve( src, dst, duration, delayDuration )
	-- faster to access a local timer var than a global one
	local transition = transition

	duration = duration or 500
	transition.to( src, { alpha=0, time=duration, delay=delayDuration, onStart=transition._dissolvePrepareSrc, onComplete=transition._setInvisible } )
	transition.to( dst, { alpha=1, time=duration, delay=delayDuration, onStart=transition._dissolvePrepareDst } )
end

function transition._createSlideParameters( target, direction, pos, duration )
	local bounds = target.stageBounds
	local dx = 0
	local dy = 0

	-- Calculate all deltas as if we're sliding "in" from offscreen (pos)
	if ( "left" == pos ) then
		dx = bounds.xMax
	elseif ( "right" == pos ) then
		dx = bounds.xMin - display.stageWidth
	elseif ( "top" == pos ) then
		dy = bounds.yMax
	else -- "bottom"
		dy = bounds.yMin - display.stageHeight
	end

	local result = nil
	if ( dx ~= 0 or dy ~= 0 ) then
		if ( "out" == direction ) then
			result = { x=(target.x-dx), y=(target.y-dy), time=duration, onComplete=transition._setInvisible }
		else
			-- store precise final positions
			result = { x=target.x, y=target.y, time=duration }

			-- init o's starting position off screen
			target:translate( -dx, -dy )
		end
	end

	return result
end

function transition.slide( src, dst, duration, dstEntryPos, style )
	-- faster to access a local timer var than a global one
	local transition = transition

-- TODO: Reorder src to be *above* dst
--	src:moveAbove( dst )

	src.isVisible = true
	dst.isVisible = true

	duration = duration or 1000

	local srcParameters = nil
	if ( style == "over" ) then
		src.alpha = 1
		srcParameters = { alpha=0, time=duration, onComplete=transition._setInvisible }
	else
		-- "push"
		local oppositePos = "top"
		if ( dstEntryPos == "left" ) then
			oppositePos = "right"
		elseif ( dstEntryPos == "right" ) then
			oppositePos = "left"
		elseif( dstEntryPos == "top" ) then
			oppositePos = "bottom"
		end
--print( "pos,oppositePos = "..dstEntryPos ..","..oppositePos )
		srcParameters = transition._createSlideParameters( src, "out", oppositePos, duration )
	end

	transition.to( src, srcParameters )
--printTable( srcParameters, "slide srcParam" )

	dstParameters = transition._createSlideParameters( dst, "in", dstEntryPos, duration )
	transition.to( dst, dstParameters )

--printTable( dstParameters, "slide dstParam" )
end

return transition