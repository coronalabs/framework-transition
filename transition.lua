-------------------------------------------------------------------------------
--
-- transition.lua
-- 
-- Version: 2.0
--
-- Copyright (C) 2013 Corona Labs Inc. All Rights Reserved.
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

-- a table holding all the sequences
lib._sequenceTable = {}

-- the last time the application was suspended
lib._prevSuspendTime = 0

-- control variable for the runtime listener
lib._didAddRuntimeListener = false

-- reserved properties that cannot be transitioned
lib._reservedProperties = {"time", "delay", "delta", "iterations", "tag", "transition", "onComplete", "onPause", "onResume", "onCancel", "onRepeat", "onStart" }

-----------------------------------------------------------------------------------------
-- local functions
-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------
-- _deepCopyObjectParameters( sourceObject, sourceParams, withDelta )
-- copies all the parameters of an object to a table it returns, with filtering of 
-- reserved properties and adjustments of delta values
-----------------------------------------------------------------------------------------
local function _deepCopyObjectParameters ( sourceObject, sourceParams, withDelta )
	-- temporary copy table
	local copyTable = {}

	-- we copy all the source object's properties
	for k, v in pairs( sourceObject ) do
		-- TODO: There should be validation of values, as is done in the 1.0 library
		-- See the following in the 1.0 library:
		--   transition._nonPropertyKeys
		--   validateValue()
		copyTable[ k ] = sourceObject[ k ]
	end

	-- TODO: This is less efficient than 1.0 implementation
    -- if any of the copied properties is reserved, we set it to nil    
	for i = 1, #lib._reservedProperties do
		copyTable[ lib._reservedProperties[ i ] ] = nil
	end
	
	-- if delta was passed in, we add the source param value to the temporary copy table respective values
	if withDelta then
		for k, v in pairs( copyTable ) do
			copyTable[ k ] = copyTable[ k ] + sourceParams[ k ]
		end
	end
	
	return copyTable
end

-----------------------------------------------------------------------------------------
-- _deepCopyParameters( sourceObject, sourceParams )
-- copies all the start parameters of an object to a table it returns
----------------------------------------------------------------------------------------- 
local function _deepCopyParameters ( sourceObject, sourceParams )
	-- temporary copy table
	local copyTable = {}
	
	-- loop and copy all the source parameters
	for k, v in pairs( sourceParams ) do
		copyTable[ k ] = sourceObject[ k ]
	end
	
	return copyTable
end

-----------------------------------------------------------------------------------------
-- _createTransitionObjectProperties( sourceParams, keywordList )
-- creates a table containing all the needed properties to operate a transition on an object (see transition.to below)
-----------------------------------------------------------------------------------------
local function _createTransitionObjectProperties( sourceParams, keywordList )
	-- temporary copy table
	local copyTable = {}
	for i = 1, #keywordList do
		-- TODO: if sourceParams[] returns nil, then grab the value from 
		-- a _defaultParams table.
		copyTable[ keywordList[ i ] ] = sourceParams[ keywordList[ i ] ]
	end
	return copyTable
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
				lib._transitionTable[ i ]._beginTransitionTime = lib._transitionTable[ i ]._beginTransitionTime + nextSuspendedTime
			end
		end
	end
end

-----------------------------------------------------------------------------------------
-- _dispatchControlEvent( targetObject, controlEvent )
-- if the controlEvent is defined in the transition object, this function dispatches that event
-- control Events: onComplete, onPause, onResume, onCancel, onRepeat, onStart
----------------------------------------------------------------------------------------- 
local function _dispatchControlEvent( targetObject, controlEvent )
	-- if the transition object does not contain this event, return
	if nil == targetObject[ controlEvent ] then
		return
	end
	
	-- if it does, execute it, sending the object as event (so we can use event.target)
	targetObject[ controlEvent ]( targetObject.target, targetObject )
end

-----------------------------------------------------------------------------------------
-- _dispatchTransitionMethod( methodCalled, filterMethod, reverseTraversal )
-- executes transition lib calls on the transition objects
-- methodCalled can be pause, resume or cancel
-- filterMethod is used to identify specific groups
-- reverseTraversal is used when cancelling all transitions, in which case we traverse the transition table from the end to the beginning, to avoid dependencies
-----------------------------------------------------------------------------------------
local function _dispatchTransitionMethod ( methodCalled, filterMethod, reverseTraversal )
	if reverseTraversal then
		for i = #lib._transitionTable, 1, -1 do
			-- if we don't have a filter method or the filter method matches the current record
			if nil == filterMethod or filterMethod( lib._transitionTable[ i ] ) then
				methodCalled( lib._transitionTable[ i ] )
			end
		end             
	else
		for i = 1, #lib._transitionTable do
			-- if we don't have a filter method or the filter method matches the current record
			if nil == filterMethod or filterMethod( lib._transitionTable[ i ] ) then
				methodCalled( lib._transitionTable[ i ] )
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
		error( DEBUG_STRING .. " you have to pass a display object to a transition.to call." )
	end
	
	if nil == transitionParams then
		error( DEBUG_STRING .. " you have to pass a params table to a transition.to call." )
	end

	-- TODO: Assignment of defaults should be done in _createTransitionObjectProperties

	-- Copy all the needed properties to the transition object
	local transitionObject = _createTransitionObjectProperties( transitionParams, lib._reservedProperties )
	
	-- Create the object properties we need in order to operate the transition properly
	-- The last time the transition was paused at
	transitionObject._lastPausedTime = nil
	
	-- Transition has completed control variable
	transitionObject._transitionHasCompleted = false
	
	-- The transition target object (used for event dispatch)
	transitionObject.target = targetObject

	-- The transition object begin time
	transitionObject._beginTransitionTime = system.getTimer()

	-- The transition object target parameters ( the end params )
	transitionObject._transitionTarget = _deepCopyObjectParameters( transitionParams )

	-- The transition object source parameters ( the begin params )
	transitionObject._transitionSource = nil

	-- The transition time ( specified in the params )
	if nil == transitionObject.time then
		transitionObject.time = 500
	end
	
	if transitionObject.time == 0 then transitionObject.time = 0.1 end
	
	-- The transition delay ( specified in the params )
	if nil == transitionObject.delay then
		transitionObject.delay = 0
	end

	-- The transition easing ( specified in the params )	
	if nil == transitionObject.transition then
		transitionObject.transition = easing.linear
	end

	-- TODO: This is not needed. 'nil' is equivalent to 'false'
	-- The transition delta ( specified in the params )
	if nil == transitionObject.delta then
		transitionObject.delta = false
	end

	-- The transition number of iterations ( specified in the params )
	if nil == transitionObject.iterations or 0 == transitionObject.iterations then
		transitionObject.iterations = 1
	end

	-- Insert the object in the transition table
	table.insert( lib._transitionTable, transitionObject )
	
	-- If we don't have a runtime listener, add it
	if not lib._didAddRuntimeListener then
		Runtime:addEventListener( "enterFrame", lib.enterFrame )
		lib._didAddRuntimeListener = "true"
	end

	return transitionObject
	
end

-----------------------------------------------------------------------------------------
-- from( targetObject, transitionParams )
-- transitions an object from the specified transitionParams
----------------------------------------------------------------------------------------- 
lib.from = function( targetObject, transitionParams )
	if nil == targetObject then
		error( DEBUG_STRING .. " you have to pass a display object to a transition.from call." )
	end
	
	if nil == transitionParams then
		error( DEBUG_STRING .. " you have to pass a params table to a transition.from call." )
	end

	local newParams = {}
	
	-- we copy the transition params from the target object and set them as final transition params
	for k, v in pairs( transitionParams ) do
		if nil ~= targetObject[ k ] then
			newParams[ k ] = targetObject[ k ]
			targetObject[ k ] = v
		else
			newParams[ k ] = v
		end
	end
                
	-- create the transition and return the object
	return lib.to( targetObject, newParams )
end
    
-----------------------------------------------------------------------------------------
-- pause( whatToPause )
-- pauses the whatToPause transition object, sequence, tag or display object
-----------------------------------------------------------------------------------------
lib.pause = function( whatToPause )
	
	-- transition object or display object
	if "table" == type( whatToPause ) then
	
		-- if the .transition field exists, then we have a transition object
		if nil ~= whatToPause.transition then
			-- if the transition is already completed, return
			if whatToPause._transitionHasCompleted then
				return
			end
	
			-- if the transition is already paused, return
			if nil ~= whatToPause._lastPausedTime then
				return
			end
				
			-- set the pausedTime to the current time
			whatToPause._lastPausedTime = system.getTimer()
	
			-- dispatch the onPause control event
			_dispatchControlEvent( whatToPause, "onPause" )
	
		-- otherwise, we have a display object
		else			
			_dispatchTransitionMethod( lib.pause, function( x ) return x.target == whatToPause end, true )
		end
	
	-- sequence name or tag
	elseif "string" == type( whatToPause ) then

		local sequenceFound = false
	
		local function f ( k, v )
			if tostring( k ) == whatToPause then
				sequenceFound = true
			end
		end

		table.foreach (lib._sequenceTable, f)
	
		-- we have a sequence
		if true == sequenceFound then
			if nil == lib._sequenceTable[ whatToPause ] then
				error( DEBUG_STRING .. " the sequence name passed to the transition.pause call does not exist." )
			end
	
		local currentSequence = lib._sequenceTable[ whatToPause ]
	
		-- pause all the transitions having the sequence object as destination
		_dispatchTransitionMethod( lib.pause, function( x ) return x.target == currentSequence.object end )
	
		-- we have a tag
		else
			-- dispatch, with filter function for the tag
			_dispatchTransitionMethod( lib.pause, function( x ) return x.tag == whatToPause end )
		end
	
	-- pause all
	elseif nil == whatToPause then
		_dispatchTransitionMethod( lib.pause, function( x ) return true end )
	end
	
end

-----------------------------------------------------------------------------------------
-- resume( whatToResume )
-- resumes the whatToResume transition object, display object, sequence, tag or nil for all
-----------------------------------------------------------------------------------------
lib.resume = function( whatToResume )

	-- transition object or display object
	if "table" == type( whatToResume ) then
	
		-- if the .transition field exists, then we have a transition object
		if nil ~= whatToResume.transition then
			-- if the transition already completed, return
			if whatToResume._transitionHasCompleted then
				return
			end
	
			-- if the transition object was never paused, return
			if nil == whatToResume._lastPausedTime then
				return
			end
				
			-- we calculate the time interval the transition was paused for
			local transitionPausedInterval = system.getTimer() - whatToResume._lastPausedTime
	
			-- we adjust the transition object's begin transition variable with the calculated time interval
			whatToResume._beginTransitionTime = whatToResume._beginTransitionTime + transitionPausedInterval
	
			-- nil out the lastPausedTime variable of the transition object
			whatToResume._lastPausedTime = nil
	
			-- dispatch the onResume method on the object
			_dispatchControlEvent( whatToResume, "onResume" )
	
		-- otherwise, we have a display object
		else			
			_dispatchTransitionMethod( lib.resume, function( x ) return x.target == whatToResume end, true )
		end
	
	-- sequence name or tag
	elseif "string" == type( whatToResume ) then

		local sequenceFound = false
	
		local function f ( k, v )
			if tostring( k ) == whatToResume then
				sequenceFound = true
			end
		end

		table.foreach (lib._sequenceTable, f)
	
		-- we have a sequence
		if true == sequenceFound then
		
			if nil == lib._sequenceTable[ whatToResume ] then
				error( DEBUG_STRING .. " the sequence name passed to the transition.resume call does not exist." )
			end
	
			local currentSequence = lib._sequenceTable[ whatToResume ]
	
			-- resume all the transitions having the sequence object as destination
			_dispatchTransitionMethod( lib.resume, function( x ) return x.target == currentSequence.object end )
	
		-- we have a tag
		else
			-- dispatch, with filter function for the tag
			_dispatchTransitionMethod( lib.resume, function( x ) return x.tag == whatToResume end )
		end
	
	-- resume all
	elseif nil == whatToResume then
		_dispatchTransitionMethod( lib.resume, function( x ) return true end )
	end

end

-----------------------------------------------------------------------------------------
-- cancel( transitionObject )
-- cancels the transitionObject transition
-----------------------------------------------------------------------------------------
lib.cancel = function( whatToCancel )

	-- transition object or display object
	if "table" == type( whatToCancel ) then
	
		-- if the .transition field exists, then we have a transition object
		if nil ~= whatToCancel.transition then

			-- if the transition completed, return
			if whatToCancel._transitionHasCompleted then
				return
			end
	
			-- set the transition as completed
			whatToCancel._transitionHasCompleted = true
	
			-- iterate the transition table and remove the transition object
			for i = 1, #lib._transitionTable do
				if lib._transitionTable[ i ] == whatToCancel then
					table.remove( lib._transitionTable, i )
					break
				end
			end
	
			-- if the table is empty, remove the event listener and set the module variable to false
			if #lib._transitionTable == 0 then
				Runtime:removeEventListener( "enterFrame", lib.enterFrame )
				lib._didAddRuntimeListener = false
			end
	
			-- dispatch onCancel on the transition object
			_dispatchControlEvent(whatToCancel, "onCancel")

	
		-- otherwise, we have a display object
		else			
			_dispatchTransitionMethod( lib.cancel, function( x ) return x.target == whatToCancel end, true )
		end
	
	-- sequence name or tag
	elseif "string" == type( whatToCancel ) then

		local sequenceFound = false
	
		local function f ( k, v )
			if tostring( k ) == whatToCancel then
				sequenceFound = true
			end
		end

		table.foreach (lib._sequenceTable, f)
	
		-- we have a sequence
		if true == sequenceFound then
		
			if nil == lib._sequenceTable[ whatToCancel ] then
				error( DEBUG_STRING .. " the sequence name passed to the transition.cancel call does not exist." )
			end
	
			local currentSequence = lib._sequenceTable[ whatToCancel ]
	
			-- pause all the transitions having the sequence object as destination
			_dispatchTransitionMethod( lib.cancel, function( x ) return x.target == currentSequence.object end )
			table.remove( lib._sequenceTable, whatToCancel )
	
		-- we have a tag
		else
			-- dispatch, with filter function for the tag
			_dispatchTransitionMethod( lib.cancel, function( x ) return x.tag == whatToCancel end, true )
		end
	
	-- resume all
	elseif nil == whatToCancel then
		_dispatchTransitionMethod( lib.cancel, function( x ) return true end )
	end

end

-- TODO: This should be renamed to _enterFrame, or better yet use a table listener
-- function lib:enterFrame(), and then Runtime:addEventListener( "enterFrame", lib )
-----------------------------------------------------------------------------------------
-- enterFrame( event )
-- the frame listener for the transitions
-----------------------------------------------------------------------------------------
lib.enterFrame = function( event )

	-- get the current event time
	local eventTime = event.time
	
	-- create a local completed transitions table which we will empty at the end of the function's execution
	local completedTransitions = {}
	
	-- iterate the transition table
	for i=1, #lib._transitionTable do 
		local currentTransitionObject = lib._transitionTable[ i ]
		
		-- if the object is not paused
		if nil == currentTransitionObject._lastPausedTime then
		
			-- calculate the time interval passed
			local passedTimeInterval = eventTime - ( currentTransitionObject._beginTransitionTime + currentTransitionObject.delay )
			
			-- if we have a time interval
			if passedTimeInterval > 0 then

				-- if we don't have source parameters for the current object, we create them, keeping account of delta
				if nil == currentTransitionObject._transitionSource then
					-- dispatch the onStart event on the transition object
					-- NOTE: This must be called prior to caching the source params
					_dispatchControlEvent( currentTransitionObject, "onStart" )

					currentTransitionObject._transitionSource = _deepCopyParameters( currentTransitionObject.target, currentTransitionObject._transitionTarget )
					if currentTransitionObject.delta then
						currentTransitionObject._transitionTarget = _deepCopyObjectParameters( currentTransitionObject._transitionTarget, currentTransitionObject.target, currentTransitionObject.delta)
					end
				end

				-- if the passed time interval is greater than the transition time, set it to that value and complete the transition
				if passedTimeInterval >= currentTransitionObject.time then
					passedTimeInterval = currentTransitionObject.time 
					currentTransitionObject._transitionHasCompleted = true
				end
				
				-- localize the parameters, to gain performance
				
				-- the transition object's target
				local currentTarget = currentTransitionObject.target
				
				-- the transition object's easing
				local currentEasing = currentTransitionObject.transition

				-- the transition source parameters
				local currentSourceParams = currentTransitionObject._transitionSource
				
				-- the transition target parameters
				local currentTargetParams = currentTransitionObject._transitionTarget
				
				-- the transition time
				local currentTargetTime = currentTransitionObject.time
				
				-- iterate the transition parameters and modify their values accordingly
				for i, x in pairs( currentTargetParams ) do
					-- calculate the diff factor (transition progress, values between 0 and 1) based on the easing					
					local diff = currentEasing( passedTimeInterval, currentTargetTime, 0, 1 )
					
					-- assign the new value to the current parameter
					if nil ~= currentSourceParams[ i ] then
						currentTarget[ i ] = ( ( x - currentSourceParams[ i ] ) * diff ) + currentSourceParams[ i ]
					end
					
				end
                                        
				-- treat the iterations
				if currentTransitionObject._transitionHasCompleted then
					-- if we only have one iteration
					if currentTransitionObject.iterations == 1 then
						-- the transition has completed
						completedTransitions[ #completedTransitions + 1 ] = i
						_dispatchControlEvent( currentTransitionObject, "onComplete" )
					else
						-- we have more iterations
						currentTransitionObject._transitionHasCompleted = false
						if currentTransitionObject.iterations > 0 then
							currentTransitionObject.iterations = currentTransitionObject.iterations-1
						end
						currentTransitionObject._beginTransitionTime = currentTransitionObject._beginTransitionTime + currentTransitionObject.time + currentTransitionObject.delay
						_dispatchControlEvent(currentTransitionObject, "onRepeat")
					end
				end
			end
		end
	end
                
	-- Remove anything transitions that is done
	for i=#completedTransitions,1,-1 do
		table.remove(lib._transitionTable, completedTransitions[i])
	end
                
	-- Be nice and preserve resources if no transitions can run
	-- TODO: Should also unregister when there are only paused transitions
	if #lib._transitionTable == 0 then
		Runtime:removeEventListener("enterFrame", lib.enterFrame)
		lib._didAddRuntimeListener = false
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
		error( DEBUG_STRING .. " you have to pass a target object to a transition.createSequence call." )
	end
	
	if params == nil then
		error( DEBUG_STRING .. " you have to pass a params table to a transition.createSequence call." )
	end

	if params.name == nil then
		error( DEBUG_STRING .. " you have to pass a name in the params table to a transition.createSequence call." )
	end

	if params.transitions == nil then
		error( DEBUG_STRING .. " you have to pass a table of transitions in the params table to a transition.createSequence call." )
	end
	
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
		error( DEBUG_STRING .. " you have to pass a sequence name to a transition.runSequence call." )
	end
	
	if nil == lib._sequenceTable[ sequenceName ] then
		error( DEBUG_STRING .. " the sequence name passed to the transition.runSequence call does not exist." )
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
		error( DEBUG_STRING .. " you have to pass a target object to a transition.blink call." )
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
	local actionXScale = paramsTable.xScale or targetObject.xScale
	local actionYScale = paramsTable.yScale or targetObject.yScale
	local actionAlpha = paramsTable.alpha or targetObject.alpha
	local actionTag = paramsTable.tag or nil
	local actionTime = actionTime or 500
	local actionDelay = actionDelay or 0
	local actionX = x or targetObject.x
	local actionY = y or targetObject.y
	
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
		xScale = actionXScale,
		yScale = actionYScale,
		x = actionX,
		y = actionY,
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
		error( DEBUG_STRING .. " you have to pass a target object to a transition.moveTo call." )
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
	local actionXScale = paramsTable.xScale or targetObject.xScale
	local actionYScale = paramsTable.yScale or targetObject.yScale
	local actionAlpha = paramsTable.alpha or targetObject.alpha
	local actionTag = paramsTable.tag or nil
	local actionX = paramsTable.x or targetObject.x
	local actionY = paramsTable.y or targetObject.y
	
	local addedTransition = lib.to( targetObject, 
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
	
	return addedTransition

  end

-----------------------------------------------------------------------------------------
-- moveBy( targetObject, xCoord, yCoord, actionTime, actionDelay )
-- moves the targetObject by xCoord, yCoord from the actual position with the transition duration actionDuration and delay actionDelay
-----------------------------------------------------------------------------------------
lib.moveBy = function( targetObject, params )
	if targetObject == nil then
		error( DEBUG_STRING .. " you have to pass a target object to a transition.moveBy call." )
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
	local actionXScale = paramsTable.xScale or targetObject.xScale
	local actionYScale = paramsTable.yScale or targetObject.yScale
	local actionAlpha = paramsTable.alpha or targetObject.alpha
	local actionTag = paramsTable.tag or nil
	local actionX = paramsTable.x or 0
	local actionY = paramsTable.y or 0
	
	local addedTransition = lib.to( targetObject, 
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
	
	return addedTransition

end

-----------------------------------------------------------------------------------------
-- scaleTo( targetObject, xScale, yScale, actionTime, actionDelay )
-- scales the targetObject to the xScale, yScale scale values with the transition duration actionDuration and delay actionDelay
-----------------------------------------------------------------------------------------
lib.scaleTo = function( targetObject, params )
	if targetObject == nil then
		error( DEBUG_STRING .. " you have to pass a target object to a transition.scaleTo call." )
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
	local actionXScale = paramsTable.xScale or targetObject.xScale
	local actionYScale = paramsTable.yScale or targetObject.yScale
	local actionAlpha = paramsTable.alpha or targetObject.alpha
	local actionX = paramsTable.x or targetObject.x
	local actionY = paramsTable.y or targetObject.y
	local actionTag = paramsTable.tag or nil
	
	local addedTransition = lib.to( targetObject, 
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

	return addedTransition

end

-----------------------------------------------------------------------------------------
-- scaleBy( targetObject, xScale, yScale, actionTime, actionDelay )
-- scales the targetObject by the xScale, yScale scale values with the transition duration actionDuration and delay actionDelay
-----------------------------------------------------------------------------------------
lib.scaleBy = function( targetObject, params )
	if targetObject == nil then
		error( DEBUG_STRING .. " you have to pass a target object to a transition.scaleBy call." )
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
	local actionXScale = paramsTable.xScale or 0
	local actionYScale = paramsTable.yScale or 0
	local actionAlpha = paramsTable.alpha or targetObject.alpha
	local actionX = paramsTable.x or targetObject.x
	local actionY = paramsTable.y or targetObject.y
	local actionTag = paramsTable.tag or nil
	
	local addedTransition = lib.to( targetObject, 
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
		
	return addedTransition

end

-----------------------------------------------------------------------------------------
-- fadeIn( targetObject, actionDuration )
-- fades in the targetObject with the transition duration actionDuration
-----------------------------------------------------------------------------------------
lib.fadeIn = function( targetObject, params )
	if targetObject == nil then
		error( DEBUG_STRING .. " you have to pass a target object to a transition.fadeIn call." )
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
	local actionX = paramsTable.x or targetObject.x
	local actionY = paramsTable.y or targetObject.y
	local actionTag = paramsTable.tag or nil
	
	local addedTransition = lib.to( targetObject, 
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
	
	return addedTransition
	
end

-----------------------------------------------------------------------------------------
-- fadeOut( targetObject, actionDuration )
-- fades out the targetObject with the transition duration actionDuration
-----------------------------------------------------------------------------------------
lib.fadeOut = function( targetObject, params )
	if targetObject == nil then
		error( DEBUG_STRING .. " you have to pass a target object to a transition.fadeIn call." )
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
	local actionX = paramsTable.x or targetObject.x
	local actionY = paramsTable.y or targetObject.y
	local actionTag = paramsTable.tag or nil
	
	local addedTransition = lib.to( targetObject, 
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