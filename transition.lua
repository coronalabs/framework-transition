-------------------------------------------------------------------------------
-- Corona Labs
--
-- transition.lua
--
-- Version: 2.0
--
-- Code is MIT licensed; see https://www.coronalabs.com/links/code/license
--
-------------------------------------------------------------------------------

local Library = require "CoronaLibrary"

-- the transition object
local lib = Library:new{ name='transition', publisherId='com.coronalabs', version=2 }

-----------------------------------------------------------------------------------------
-- constants
-----------------------------------------------------------------------------------------

local DEBUG_STRING = "Transition 2.0: "

-----------------------------------------------------------------------------------------
-- lib variables
-----------------------------------------------------------------------------------------

-- a table holding all transitions, active or paused
lib._transitionTable = {}

-- a table holding all the transitions, to be iterated by the pause / resume / cancel methods
lib._enterFrameTweens = {}

-- a table holding all the sequences
lib._sequenceTable = {}

-- the last time the application was suspended
lib._prevSuspendTime = 0

-- control variable for the runtime listener
lib._hasEventListener = false

-- reserved properties that cannot be transitioned
lib._reservedProperties =
{
	time = true, delay = true, delta = true, iterations = true, tag = true, transition = true,
	onComplete = true, onPause = true, onResume = true, onCancel = true, onRepeat = true, onStart = true
}

-- keys that have a number value
lib._numberKeys =
{
	x=true, y=true, xScale=true, yScale=true, rotation=true, width=true, height=true,
	alpha=true,
	xReference=true, yReference=true,
	maskX=true, maskY=true, maskScaleX=true, maskScaleY=true, maskRotation=true,
	delta=true,
}

lib.debugEnabled = false

-- have pause(), resume() or cancel() API calls ignore empty (nil) reference
lib.ignoreEmptyReference = false

-----------------------------------------------------------------------------------------
-- local functions
-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------
-- _validateValue( k, v )
-- validates input values. Ensures they are number format
-----------------------------------------------------------------------------------------
local function _validateValue( k, v )
	if ( "number" == type( v ) and lib._numberKeys[k] ) then
		local isNan = ( v ~= v )
		if ( isNan ) then
			print( "ERROR: Invalid number argument provided. Attempt to set property " .. k .. " to NaN." )
		end
		assert( not isNan )
	end

	return v
end

-----------------------------------------------------------------------------------------
-- copyTable( src )
-- copies the contents of a table to another table
-----------------------------------------------------------------------------------------
local function _copyTable( src )
	local t = {}

	for k,v in pairs( src ) do
		t[k] = v
	end

	return t
end

-----------------------------------------------------------------------------------------
-- _initTween( tween, parameters )
-- assigns the start and end values of a tween, accounting for delta and valid values
-----------------------------------------------------------------------------------------
lib._initTween = function( tween, parameters )
	local target = tween._target
	local listener = tween._onStart
	if listener then
		Runtime.callListener( listener, "onStart", target )
		tween._onStart = nil -- only call once
	end

	local keysStart = {}
	local keysFinish = {}
	local invalidKeys = lib._reservedProperties
	local isDeltaValue = _validateValue( "delta", parameters.delta )
	for k,v in pairs( parameters ) do
		if not lib._reservedProperties[k] then
			-- Only tween properties that have a non-nil starting value
			local startValue = target[k]
			if startValue then
				keysStart[k] = startValue
				local finishValue = (isDeltaValue and (startValue+v)) or v
				keysFinish[k] = _validateValue( k, finishValue )
			end
		end
	end
	tween._keysStart = keysStart
	tween._keysFinish = keysFinish
end

-----------------------------------------------------------------------------------------
-- _addTween( tween )
-- inserts a tween to the active tweens table. Sets up the enterFrame listener if
-- the added tween is the first one
-----------------------------------------------------------------------------------------
lib._addTween = function( tween )
	local activeTweens = lib._transitionTable

	-- Check for any tweens that are already moved to enterframe from lib._transitionTable
	if #activeTweens == 0 and #lib._enterFrameTweens > 0 then
		activeTweens = lib._enterFrameTweens
	end

	-- Once we have at least one tween, register for frame events
	if #activeTweens == 0 and not lib._hasEventListener then
		lib._hasEventListener = true
		Runtime:addEventListener( "enterFrame", lib )
	end

	-- Set a flag so we can mark the tween
	tween._instantiatedByTransLib = true

	table.insert( activeTweens, tween )
end

-----------------------------------------------------------------------------------------
-- _handleSuspendResume( event )
-- handles the suspending / resuming of transitions in case of suspend / resume events
-----------------------------------------------------------------------------------------
local function _handleSuspendResume( event )
	-- if the application got suspended
	if "applicationSuspend" == event.type then
		-- assign the prevSuspendTime variable to the current internal time
		lib._prevSuspendTime = system.getTimer()

	-- if the application resumed
	elseif "applicationResume" == event.type then

		-- calculate the difference between the suspension time and the current internal time
		local nextSuspendedTime = system.getTimer() - lib._prevSuspendTime

		-- assign the difference to all the transitions that are in the table
		for i = 1, #lib._transitionTable do
			-- only do this for non-completed transitions
			if not lib._transitionTable[ i ]._transitionHasCompleted then
				lib._transitionTable[ i ]._timeStart = lib._transitionTable[ i ]._timeStart + nextSuspendedTime
			end
		end
	end
end

-----------------------------------------------------------------------------------------
-- to( targetObject, transitionParams )
-- transitions an object to the specified transitionParams
-----------------------------------------------------------------------------------------
lib.to = function( targetObject, transitionParams )
	if nil == targetObject then
		if lib.debugEnabled then
			error( DEBUG_STRING .. " you have to pass a display object to a transition.to call." )
		end
	end

	if nil == transitionParams then
		if lib.debugEnabled then
			error( DEBUG_STRING .. " you have to pass a params table to a transition.to call." )
		end
	end

	local tween = nil

	if targetObject and transitionParams then
		-- faster to access a local timer var than a global one
		local lib = lib

		tween = {}
		local t = system.getTimer()
		tween._target = targetObject

		tween._timeStart = t
		tween._duration = transitionParams.time or 500
		tween.iterations = transitionParams.iterations or 1
		tween.tag = transitionParams.tag or ""
		tween._lastPausedTime = nil
		tween._transition = transitionParams.transition or easing.linear
		tween._onStart = Runtime.verifyListener( transitionParams.onStart, "onStart" )
		tween._onComplete = Runtime.verifyListener( transitionParams.onComplete, "onComplete" )
		tween._onPause = Runtime.verifyListener( transitionParams.onPause, "onPause" )
		tween._onResume = Runtime.verifyListener( transitionParams.onResume, "onResume" )
		tween._onCancel = Runtime.verifyListener( transitionParams.onCancel, "onCancel" )
		tween._onRepeat = Runtime.verifyListener( transitionParams.onRepeat, "onRepeat" )
		if transitionParams.generatedBy and transitionParams.generatedBy == "composer" then
			tween._generatedByComposer = true
		end

		local delay = transitionParams.delay
		if type(delay) == "number" and delay ~= 0 then
			tween._delay = delay
			-- save off params: init after delay to minimize race conditions
			local params = _copyTable( transitionParams )
			tween._delayParams = params
		else
			-- no delay, so init immediately
			lib._initTween( tween, transitionParams )
		end

		lib._addTween( tween )
	end

	return tween



end

-----------------------------------------------------------------------------------------
-- from( targetObject, transitionParams )
-- transitions an object from the specified transitionParams
-----------------------------------------------------------------------------------------
lib.from = function( targetObject, transitionParams )
	if nil == targetObject then
		if lib.debugEnabled then
			error( DEBUG_STRING .. " you have to pass a display object to a transition.from call." )
		end
	end

	if nil == transitionParams then
		if lib.debugEnabled then
			error( DEBUG_STRING .. " you have to pass a params table to a transition.from call." )
		end
	end

	local newParams = {}

	if targetObject and transitionParams then

		local isDelta = transitionParams.delta

		-- we copy the transition params from the target object and set them as final transition params
		for k, v in pairs( transitionParams ) do
			if targetObject[ k ] then

				local startPos, endPos
				if isDelta then
					endPos = - v
					startPos = v + targetObject[ k ]
				else
					endPos = targetObject[ k ]
					startPos = v
				end

				newParams[ k ] = endPos
				targetObject[ k ] = startPos
			else
				newParams[ k ] = v
			end
		end

	end

	-- create the transition and return the object
	return lib.to( targetObject, newParams )
end


lib.loop = function( targetObject, transitionParams )
	local tween = lib.to(targetObject, transitionParams)
	tween._isLoop = true
	return tween
end

-----------------------------------------------------------------------------------------
-- pause( whatToPause )
-- pauses the whatToPause transition object, sequence, tag or display object
-----------------------------------------------------------------------------------------
lib.pause = function( whatToPause, _pauseAll )

	-- we use the targetType variable to establish how we iterate at the end of this method
	local targetType = nil
	local iterationTarget = nil

	-- transition object or display object
	if "table" == type( whatToPause ) then
		-- if the ._instantiatedByTransLib field exists, then we have a transition object
		if whatToPause._instantiatedByTransLib then
			targetType = "transition"
			whatToPause._paused = true
		-- otherwise, we have a display object
		else
			targetType = "displayobject"
		end
	-- sequence name or tag
	elseif "string" == type( whatToPause ) then
		targetType = "tag"
	-- pause all
	elseif nil == whatToPause and not lib.ignoreEmptyReference or _pauseAll then
		targetType = "all"
	end

	if targetType then
		if "all" ~= targetType then iterationTarget = whatToPause end

		local transitionTable = lib._transitionTable

		-- if enterframe was run already, if we have any running tweens they won't be in the lib._transitionTable variable, but in lib._enterFrameTweens
		if #transitionTable == 0 then
			transitionTable = lib._enterFrameTweens
		end

		-- iterate the table
		for i,tween in ipairs( transitionTable ) do
			-- only cancel if the tween was not generated by the composer library
			if targetType and not tween._generatedByComposer then
				if targetType == "all" then
					tween._paused = true
					tween._resume = false
				elseif targetType == "tag" then
					if tween.tag == iterationTarget then
						tween._paused = true
						tween._resume = false
					end
				elseif targetType == "displayobject" then
					if tween._target == iterationTarget then
						tween._paused = true
						tween._resume = false
					end
				elseif targetType == "transition" then
					if tween == iterationTarget then
						tween._paused = true
						tween._resume = false
					end
				end
			end

			-- dispatch the onPause control event
			local listener = tween._onPause
			if listener and tween._paused and not tween._cancelled and not tween._pauseTriggered then
				local target = tween._target
				tween._pauseTriggered = true
				tween._resumeTriggered = false
				Runtime.callListener( listener, "onPause", target )
			end

		end
	end
end

-----------------------------------------------------------------------------------------
-- resume( whatToResume )
-- resumes the whatToResume transition object, display object, sequence, tag or nil for all
-----------------------------------------------------------------------------------------
lib.resume = function( whatToResume, _resumeAll )

	-- we use the targetType variable to establish how we iterate at the end of this method
	local targetType = nil
	local iterationTarget = nil

	-- transition object or display object
	if "table" == type( whatToResume ) then
		-- if the ._instantiatedByTransLib field exists, then we have a transition object
		if whatToResume._instantiatedByTransLib then
			targetType = "transition"
			whatToResume._resume = true
		-- otherwise, we have a display object
		else
			targetType = "displayobject"
		end
	-- sequence name or tag
	elseif "string" == type( whatToResume ) then
		targetType = "tag"
	-- pause all
	elseif nil == whatToResume and not lib.ignoreEmptyReference or _resumeAll then
		targetType = "all"
	end

	if targetType then
		if "all" ~= targetType then iterationTarget = whatToResume end
		-- iterate the table

		local transitionTable = lib._transitionTable

		-- if enterframe was run already, if we have any running tweens they won't be in the lib._transitionTable variable, but in lib._enterFrameTweens
		if #transitionTable == 0 then
			transitionTable = lib._enterFrameTweens
		end

		for i,tween in ipairs( transitionTable ) do
			-- check for any resume lib variables
			-- only resume if the tween was not generated by the composer library
			if targetType and not tween._generatedByComposer then
				if targetType == "all" then
					tween._resume = true
					tween._paused = false
				elseif targetType == "tag" then
					if tween.tag == iterationTarget then
						tween._resume = true
						tween._paused = false
					end
				elseif targetType == "displayobject" then
					if tween._target == iterationTarget then
						tween._resume = true
						tween._paused = false
					end
				elseif targetType == "transition" then
					if tween == whatToResume then
						tween._resume = true
						tween._paused = false
					end
				end
			end

			-- dispatch the onResume method on the object
			local listener = tween._onResume
			if listener and tween._resume and not tween._cancelled and not tween._resumeTriggered then
				local target = tween._target
				tween._pauseTriggered = false
				tween._resumeTriggered = true
				Runtime.callListener( listener, "onResume", target )
			end
		end
	end
end

-----------------------------------------------------------------------------------------
-- cancel( transitionObject )
-- cancels the transitionObject transition
-----------------------------------------------------------------------------------------
lib.cancel = function( whatToCancel, _cancelAll )

	-- if no transitions, then don't cancel anything
	-- this is important because if one calls transition.cancel("tagname") followed by transition.to( obj, { tag = "tagname" } )
	-- the newly created transition would be cancelled automatically.

	local cancelType, cancelTarget

	-- transition object or display object
	if "table" == type( whatToCancel ) then
		if whatToCancel._instantiatedByTransLib then
			-- set the tween's _cancelled property
			whatToCancel._cancelled = true
		else
			cancelType = "displayobject"
			cancelTarget = whatToCancel
		end
	-- sequence name or tag
	elseif "string" == type( whatToCancel ) then
			cancelType = "tag"
			cancelTarget = whatToCancel
	-- cancel all
	elseif nil == whatToCancel and not lib.ignoreEmptyReference or _cancelAll then
		cancelType = "all"
		cancelTarget = nil
	end

	local transitionTable = lib._transitionTable
	-- if enterframe was run already, if we have any running tweens they won't be in the lib._transitionTable variable, but in lib._enterFrameTweens
	if #transitionTable == 0 then
		transitionTable = lib._enterFrameTweens
	end

	for i,tween in ipairs( transitionTable ) do
		-- check for any cancel lib variables
		-- only cancel if the tween was not generated by the composer library
		if cancelType and not tween._generatedByComposer then
			if cancelType == "all" then
				tween._cancelled = true
			elseif cancelType == "tag" then
				if tween.tag == cancelTarget then
					tween._cancelled = true
				end
			elseif cancelType == "displayobject" then
				if tween._target == cancelTarget then
					tween._cancelled = true
				end
			end
		end

		-- dispatch the onCancel control event
		local listener = tween._onCancel
		if listener and tween._cancelled and not tween._cancelTriggered then
			local target = tween._target
			tween._cancelTriggered = true
			Runtime.callListener( listener, "onCancel", target )
		end
	end

end

lib.pauseAll = function()
	lib.pause( nil, true )
end

lib.resumeAll = function()
	lib.resume( nil, true )
end

lib.cancelAll = function()
	lib.cancel( nil, true )
end

-- function lib:enterFrame(), and then Runtime:addEventListener( "enterFrame", lib )
-----------------------------------------------------------------------------------------
-- enterFrame( event )
-- the frame listener for the transitions
-----------------------------------------------------------------------------------------
function lib:enterFrame ( event )

	-- create a local copy of the transition table, to avoid a race condition
	local currentActiveTweens = lib._transitionTable
	lib._enterFrameTweens = lib._transitionTable

	lib._transitionTable = {}

	-- get the current event time
	local currentTime = event.time

	-- create a local completed transitions table which we will empty at the end of the function's execution
	local completedTransitions = {}

	-- iterate the transition table
	for i,tween in ipairs( currentActiveTweens ) do
		-- if the transition object is paused
		if tween._paused then
			-- handle tweens marked as paused

			-- only set the pausedTime if that did not happen already
			if nil == tween._lastPausedTime then
				-- set the pausedTime to the current time
				tween._lastPausedTime = system.getTimer()
			end
		end

		if tween._resume then
			if not tween._transitionHasCompleted and tween._lastPausedTime then
				-- we calculate the time interval the transition was paused for
				local transitionPausedInterval = system.getTimer() - tween._lastPausedTime

				-- we adjust the transition object's begin transition variable with the calculated time interval
				tween._timeStart = tween._timeStart + transitionPausedInterval

				-- nil out the lastPausedTime variable of the transition object
				tween._lastPausedTime = nil
				tween._paused = nil
				tween._resume = nil
			end
		end

		if tween._cancelled then
			table.insert( completedTransitions, i )
		end

		if not tween._paused and not tween._cancelled then
			local delay = tween._delay
			if delay and ( currentTime >= (tween._timeStart + delay) ) then
				tween._delay = nil
				tween._timeStart = currentTime
				delay = nil
			end

			if not delay then
				local params = tween._delayParams
				if params then
					lib._initTween( tween, params )
					tween._delayParams = nil
				end

				local target = tween._target
				local keysFinish = tween._keysFinish
				local t = currentTime - tween._timeStart
				if t < 0 then t = 0 end
				local tMax = tween._duration
				if t < tMax then
          if tween._isLoop then
						t = t*2
						if tMax < t then
							t = tMax*2 - t
						end
					end

					for k,v in pairs( tween._keysStart ) do
						target[k] = tween._transition( t, tMax, v, keysFinish[k] - v )
					end
				else
					-- the easing function easing.continuousLoop with infinite iterations cannot set the object keys to the finish values.
					-- also, the last iteration of a transition with easing.continousLoop has to return the object to the start properties,
					-- not to the end ones.
					if tween._transition == easing.continuousLoop or tween._isLoop then
						if tween.iterations == 1 then
							for k, v in pairs( tween._keysStart ) do
								target[k] = v
							end
						end
					else
						for k,v in pairs( keysFinish ) do
							target[k] = v
						end
					end

					if tween.iterations == 1 then
						-- the transition has completed
						-- onComplete listener
						local listener = tween._onComplete
						if listener then
							Runtime.callListener( listener, "onComplete", target )
						end

						-- queue up complete tween for removal
						table.insert( completedTransitions, i )
					else
						-- we have more iterations
						tween._transitionHasCompleted = false
						if tween.iterations > 0 then
							tween.iterations = tween.iterations-1
						end
						tween._timeStart = tween._timeStart + tMax
						-- onRepeat listener
						local listener = tween._onRepeat
						if listener then
							Runtime.callListener( listener, "onRepeat", target )
						end
					end


				end
			end

		end

	end

	-- Remove the transitions that are done
	for i=#completedTransitions,1,-1 do
		table.remove(currentActiveTweens, completedTransitions[i])
	end

	-- Swap out tmp tween table and restore lib._transitionTable.
	-- Append any new tweens in the tmp table back into lib._transitionTable
	local tmpTweens = lib._transitionTable
	if #tmpTweens > 0 then
		for _,tween in ipairs( tmpTweens ) do
			table.insert( currentActiveTweens, tween )
		end
	end
	lib._transitionTable = currentActiveTweens
	-- also restore the _enterFrameTweens variable here
	lib._enterFrameTweens = currentActiveTweens

	-- TODO: Should also unregister when there are only paused transitions
	if #currentActiveTweens == 0 then
		Runtime:removeEventListener( "enterFrame", lib )
		lib._hasEventListener = false
	end
end

-----------------------------------------------------------------------------------------
-- createSequence( targetObject, sequenceData )
-- create a transition sequence
-- targetObject is the display object to create the sequence on
-- sequenceData is a table, containing name - the sequence name, and transitions - the list of transitions
-- for a sequence transition, you pass in an extra parameter, mode = withPrevious or afterPrevious, which defines
-- the execution order of the transitions.
-----------------------------------------------------------------------------------------
lib.newSequence = function( targetObject, params )
	if targetObject == nil then
		if lib.debugEnabled then
			error( DEBUG_STRING .. " you have to pass a target object to a transition.createSequence call." )
		end
	end

	if params == nil then
		if lib.debugEnabled then
			error( DEBUG_STRING .. " you have to pass a params table to a transition.createSequence call." )
		end
	end

	if params.name == nil then
		if lib.debugEnabled then
			error( DEBUG_STRING .. " you have to pass a name in the params table to a transition.createSequence call." )
		end
	end

	if params.transitions == nil then
		if lib.debugEnabled then
			error( DEBUG_STRING .. " you have to pass a table of transitions in the params table to a transition.createSequence call." )
		end
	end

	if targetObject and params and params.transitions and params.name then

		-- create a sequence with the name params.name
		lib._sequenceTable[params.name] = {}

		-- assign the transitions to it
		lib._sequenceTable[params.name].transitions = params.transitions

		-- assign the target object to it
		lib._sequenceTable[params.name].object = targetObject

		-- localize it
		local currentSequence = lib._sequenceTable[params.name]

		-- create a temp table for the delays
		local tranDelays = {}

		for i = 1, #currentSequence.transitions do

			local delayValue = 0

			if currentSequence.transitions[ i ].delay then
				delayValue  = delayValue + currentSequence.transitions[ i ].delay
			end

			-- if we are at least at the second transition in the table
			if i > 1 then

				for j = i - 1, 1, -1 do

					local addedDelay = 0
					local prevDelay = 0

					if currentSequence.transitions[ j ].delay then
						addedDelay = currentSequence.transitions[ j ].delay
					end

					if currentSequence.transitions[ j ].mode ~= "withPrevious" then
						prevDelay = prevDelay + currentSequence.transitions[ j ].time
					end

					prevDelay = prevDelay + addedDelay

					delayValue = delayValue + prevDelay


				end

				if currentSequence.transitions[ i ].mode == "withPrevious" then
					delayValue = delayValue - currentSequence.transitions[ i - 1 ].time
					if currentSequence.transitions[ i - 1 ].delay then
						delayValue = delayValue - currentSequence.transitions[ i - 1 ].delay
					end
				end

				--currentSequence.transitions[ i ].delay = delayValue
				tranDelays[ i ] = delayValue

			end

		end

	end

	-- assign the values from the temp table
	for i = 1, #tranDelays do
		currentSequence.transitions[ i ].delay = tranDelays[ i ]
	end

end

-----------------------------------------------------------------------------------------
-- runSequence( sequenceName )
-- runs the sequence sequenceName
-----------------------------------------------------------------------------------------
lib.runSequence = function( sequenceName )
	if sequenceName == nil then
		if lib.debugEnabled then
			error( DEBUG_STRING .. " you have to pass a sequence name to a transition.runSequence call." )
		end
	end

	if nil == lib._sequenceTable[ sequenceName ] then
		if lib.debugEnabled then
			error( DEBUG_STRING .. " the sequence name passed to the transition.runSequence call does not exist." )
		end
	end

	local currentSequence = lib._sequenceTable[ sequenceName ]

	for i, v in ipairs ( lib._sequenceTable[ sequenceName ].transitions ) do
		v.mode = nil
		lib.to( lib._sequenceTable[ sequenceName ].object, v)
	end

end

-----------------------------------------------------------------------------------------
-- convenience methods
-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------
-- blink( targetObject, actionDuration )
-- blinks the targetObject with the transition duration actionDuration
-----------------------------------------------------------------------------------------
lib.blink = function( targetObject, params )
	if targetObject == nil then
		if lib.debugEnabled then
			error( DEBUG_STRING .. " you have to pass a target object to a transition.blink call." )
		end
	end

	local paramsTable = params or {}

	local actionTime = paramsTable.time or 500
	local actionDelay = paramsTable.delay or 0
	local actionEasing = paramsTable.transition or easing.linear
	local actionOnComplete = paramsTable.onComplete or nil
	local actionOnPause = paramsTable.onPause or nil
	local actionOnResume = paramsTable.onResume or nil
	local actionOnCancel = paramsTable.onCancel or nil
	local actionOnStart = paramsTable.onStart or nil
	local actionOnRepeat = paramsTable.onRepeat or nil
	local actionTag = paramsTable.tag or nil
	local actionTime = actionTime or 500

	local addedTransition = lib.to( targetObject,
	{
		delay = actionDelay,
		time = actionTime * 0.5,
		transition = easing.continousLoop,
		iterations = -1,
		onComplete = actionOnComplete,
		onPause = actionOnPause,
		onResume = actionOnResume,
		onCancel = actionOnCancel,
		onStart = actionOnStart,
		onRepeat = actionOnRepeat,
		alpha = 0,
		tag = actionTag
	} )
		--local addedTransition = lib.to( targetObject, { time = actionTime * 0.5, alpha = 0, transition="continuousLoop", iterations = -1 } )

	return addedTransition

end

-----------------------------------------------------------------------------------------
-- moveTo( targetObject, xCoord, yCoord, actionTime, actionDelay )
-- moves the targetObject to the xCoord, yCoord coordinates with the transition duration actionDuration and delay actionDelay
-----------------------------------------------------------------------------------------
lib.moveTo = function( targetObject, params )
	if targetObject == nil then
		if lib.debugEnabled then
			error( DEBUG_STRING .. " you have to pass a target object to a transition.moveTo call." )
		end
	end

	local paramsTable = params or {}
	local addedTransition = nil

	if targetObject then

		local actionTime = paramsTable.time or 500
		local actionDelay = paramsTable.delay or 0
		local actionEasing = paramsTable.transition or easing.linear
		local actionOnComplete = paramsTable.onComplete or nil
		local actionOnPause = paramsTable.onPause or nil
		local actionOnResume = paramsTable.onResume or nil
		local actionOnCancel = paramsTable.onCancel or nil
		local actionOnStart = paramsTable.onStart or nil
		local actionOnRepeat = paramsTable.onRepeat or nil
		local actionXScale = paramsTable.xScale or nil
		local actionYScale = paramsTable.yScale or nil
		local actionAlpha = paramsTable.alpha or nil
		local actionTag = paramsTable.tag or nil
		local actionX = paramsTable.x or nil
		local actionY = paramsTable.y or nil

		addedTransition = lib.to( targetObject,
		{
			delay = actionDelay,
			time = actionTime,
			transition = actionEasing,
			onComplete = actionOnComplete,
			onPause = actionOnPause,
			onResume = actionOnResume,
			onCancel = actionOnCancel,
			onStart = actionOnStart,
			onRepeat = actionOnRepeat,
			alpha = actionAlpha,
			xScale = actionXScale,
			yScale = actionYScale,
			x = actionX,
			y = actionY,
			tag = actionTag
		} )

	end

	return addedTransition

  end

-----------------------------------------------------------------------------------------
-- moveBy( targetObject, xCoord, yCoord, actionTime, actionDelay )
-- moves the targetObject by xCoord, yCoord from the actual position with the transition duration actionDuration and delay actionDelay
-----------------------------------------------------------------------------------------
lib.moveBy = function( targetObject, params )
	if targetObject == nil then
		if lib.debugEnabled then
			error( DEBUG_STRING .. " you have to pass a target object to a transition.moveBy call." )
		end
	end

	local paramsTable = params or {}
	local addedTransition = nil

	if targetObject then

		local actionTime = paramsTable.time or 500
		local actionDelay = paramsTable.delay or 0
		local actionEasing = paramsTable.transition or easing.linear
		local actionOnComplete = paramsTable.onComplete or nil
		local actionOnPause = paramsTable.onPause or nil
		local actionOnResume = paramsTable.onResume or nil
		local actionOnCancel = paramsTable.onCancel or nil
		local actionOnStart = paramsTable.onStart or nil
		local actionOnRepeat = paramsTable.onRepeat or nil
		local actionXScale = paramsTable.xScale or nil
		local actionYScale = paramsTable.yScale or nil
		local actionAlpha = paramsTable.alpha or nil
		local actionTag = paramsTable.tag or nil
		local actionX = paramsTable.x or 0
		local actionY = paramsTable.y or 0

		addedTransition = lib.to( targetObject,
		{
			delay = actionDelay,
			time = actionTime,
			transition = actionEasing,
			onComplete = actionOnComplete,
			onPause = actionOnPause,
			onResume = actionOnResume,
			onCancel = actionOnCancel,
			onStart = actionOnStart,
			onRepeat = actionOnRepeat,
			alpha = actionAlpha,
			xScale = actionXScale,
			yScale = actionYScale,
			x = targetObject.x + actionX,
			y = targetObject.y + actionY,
			tag = actionTag
		} )

	end

	return addedTransition

end

-----------------------------------------------------------------------------------------
-- scaleTo( targetObject, xScale, yScale, actionTime, actionDelay )
-- scales the targetObject to the xScale, yScale scale values with the transition duration actionDuration and delay actionDelay
-----------------------------------------------------------------------------------------
lib.scaleTo = function( targetObject, params )
	if targetObject == nil then
		if lib.debugEnabled then
			error( DEBUG_STRING .. " you have to pass a target object to a transition.scaleTo call." )
		end
	end

	local paramsTable = params or {}
	local addedTransition = nil

	if targetObject then

		local actionTime = paramsTable.time or 500
		local actionDelay = paramsTable.delay or 0
		local actionEasing = paramsTable.transition or easing.linear
		local actionOnComplete = paramsTable.onComplete or nil
		local actionOnPause = paramsTable.onPause or nil
		local actionOnResume = paramsTable.onResume or nil
		local actionOnCancel = paramsTable.onCancel or nil
		local actionOnStart = paramsTable.onStart or nil
		local actionOnRepeat = paramsTable.onRepeat or nil
		local actionXScale = paramsTable.xScale or nil
		local actionYScale = paramsTable.yScale or nil
		local actionAlpha = paramsTable.alpha or nil
		local actionX = paramsTable.x or nil
		local actionY = paramsTable.y or nil
		local actionTag = paramsTable.tag or nil

		addedTransition = lib.to( targetObject,
		{
			delay = actionDelay,
			time = actionTime,
			transition = actionEasing,
			onComplete = actionOnComplete,
			onPause = actionOnPause,
			onResume = actionOnResume,
			onCancel = actionOnCancel,
			onStart = actionOnStart,
			onRepeat = actionOnRepeat,
			alpha = actionAlpha,
			xScale = actionXScale,
			yScale = actionYScale,
			x = actionX,
			y = actionY,
			tag = actionTag
		} )

	end

	return addedTransition

end

-----------------------------------------------------------------------------------------
-- scaleBy( targetObject, xScale, yScale, actionTime, actionDelay )
-- scales the targetObject by the xScale, yScale scale values with the transition duration actionDuration and delay actionDelay
-----------------------------------------------------------------------------------------
lib.scaleBy = function( targetObject, params )
	if targetObject == nil then
		if lib.debugEnabled then
			error( DEBUG_STRING .. " you have to pass a target object to a transition.scaleBy call." )
		end
	end

	local paramsTable = params or {}
	local addedTransition = nil

	if targetObject then

		local actionTime = paramsTable.time or 500
		local actionDelay = paramsTable.delay or 0
		local actionEasing = paramsTable.transition or easing.linear
		local actionOnComplete = paramsTable.onComplete or nil
		local actionOnPause = paramsTable.onPause or nil
		local actionOnResume = paramsTable.onResume or nil
		local actionOnCancel = paramsTable.onCancel or nil
		local actionOnStart = paramsTable.onStart or nil
		local actionOnRepeat = paramsTable.onRepeat or nil
		local actionXScale = paramsTable.xScale or 0
		local actionYScale = paramsTable.yScale or 0
		local actionAlpha = paramsTable.alpha or nil
		local actionX = paramsTable.x or nil
		local actionY = paramsTable.y or nil
		local actionTag = paramsTable.tag or nil

		addedTransition = lib.to( targetObject,
		{
			delay = actionDelay,
			time = actionTime,
			transition = actionEasing,
			onComplete = actionOnComplete,
			onPause = actionOnPause,
			onResume = actionOnResume,
			onCancel = actionOnCancel,
			onStart = actionOnStart,
			onRepeat = actionOnRepeat,
			x = actionX,
			y = actionY,
			alpha = actionAlpha,
			xScale = targetObject.xScale + actionXScale,
			yScale = targetObject.yScale + actionYScale,
			tag = actionTag
		} )

	end

	return addedTransition

end

-----------------------------------------------------------------------------------------
-- fadeIn( targetObject, actionDuration )
-- fades in the targetObject with the transition duration actionDuration
-----------------------------------------------------------------------------------------
lib.fadeIn = function( targetObject, params )
	if targetObject == nil then
		if lib.debugEnabled then
			error( DEBUG_STRING .. " you have to pass a target object to a transition.fadeIn call." )
		end
	end

	local paramsTable = params or {}
	local addedTransition = nil

	if targetObject then

		local actionTime = paramsTable.time or 500
		local actionDelay = paramsTable.delay or 0
		local actionEasing = paramsTable.transition or easing.linear
		local actionOnComplete = paramsTable.onComplete or nil
		local actionOnPause = paramsTable.onPause or nil
		local actionOnResume = paramsTable.onResume or nil
		local actionOnCancel = paramsTable.onCancel or nil
		local actionOnStart = paramsTable.onStart or nil
		local actionOnRepeat = paramsTable.onRepeat or nil
		local actionX = paramsTable.x or nil
		local actionY = paramsTable.y or nil
		local actionTag = paramsTable.tag or nil

		addedTransition = lib.to( targetObject,
		{
			delay = actionDelay,
			time = actionTime,
			transition = actionEasing,
			onComplete = actionOnComplete,
			onPause = actionOnPause,
			onResume = actionOnResume,
			onCancel = actionOnCancel,
			onStart = actionOnStart,
			onRepeat = actionOnRepeat,
			x = actionX,
			y = actionY,
			alpha = 1.0,
			tag = actionTag
		} )

	end

	return addedTransition

end

-----------------------------------------------------------------------------------------
-- fadeOut( targetObject, actionDuration )
-- fades out the targetObject with the transition duration actionDuration
-----------------------------------------------------------------------------------------
lib.fadeOut = function( targetObject, params )
	if targetObject == nil then
		if lib.debugEnabled then
			error( DEBUG_STRING .. " you have to pass a target object to a transition.fadeIn call." )
		end
	end

	local paramsTable = params or {}
	local addedTransition = nil

	if targetObject then

		local actionTime = paramsTable.time or 500
		local actionDelay = paramsTable.delay or 0
		local actionEasing = paramsTable.transition or easing.linear
		local actionOnComplete = paramsTable.onComplete or nil
		local actionOnPause = paramsTable.onPause or nil
		local actionOnResume = paramsTable.onResume or nil
		local actionOnCancel = paramsTable.onCancel or nil
		local actionOnStart = paramsTable.onStart or nil
		local actionOnRepeat = paramsTable.onRepeat or nil
		local actionX = paramsTable.x or nil
		local actionY = paramsTable.y or nil
		local actionTag = paramsTable.tag or nil

		addedTransition = lib.to( targetObject,
		{
			delay = actionDelay,
			time = actionTime,
			transition = actionEasing,
			onComplete = actionOnComplete,
			onPause = actionOnPause,
			onResume = actionOnResume,
			onCancel = actionOnCancel,
			onStart = actionOnStart,
			onRepeat = actionOnRepeat,
			tag = actionTag,
			x = actionX,
			y = actionY,
			alpha = 0.0
		} )

	end

	return addedTransition

end

-----------------------------------------------------------------------------------------
-- [Deprecated]
--
-- dissolve( src, dst, duration, delayDuration )
-- fades out src and fades in dst
-----------------------------------------------------------------------------------------

lib._setInvisible = function( object )
	object.isVisible = false
end

lib._dissolvePrepareSrc = function( object )
	object.alpha = 1
end

lib._dissolvePrepareDst = function( object )
	object.alpha = 0
	object.isVisible = true
end

lib.dissolve = function( src, dst, duration, delayDuration )
	-- faster to access a local timer var than a global one
	local lib = lib

	duration = duration or 500

	lib.to( src, { alpha=0, time=duration, delay=delayDuration, onStart=lib._dissolvePrepareSrc, onComplete=lib._setInvisible } )
	lib.to( dst, { alpha=1, time=duration, delay=delayDuration, onStart=lib._dissolvePrepareDst } )
end


-----------------------------------------------------------------------------------------
-- setup before returning
-----------------------------------------------------------------------------------------

-- add the suspend / resume event listener to the runtime object
Runtime:addEventListener("system", _handleSuspendResume)

return lib
