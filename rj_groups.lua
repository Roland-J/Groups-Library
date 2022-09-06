--[[Copyright © 2022, RolandJ
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

	* Redistributions of source code must retain the above copyright
	  notice, this list of conditions and the following disclaimer.
	* Redistributions in binary form must reproduce the above copyright
	  notice, this list of conditions and the following disclaimer in the
	  documentation and/or other materials provided with the distribution.
	* Neither the name of RolandJ Groups nor the
	  names of its contributors may be used to endorse or promote products
	  derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL RolandJ BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.]]

-- Name:    RolandJ Groups (rj_groups.lua)
-- Version: 0.0.4

images = require('images')
texts = require('texts')
require('tables')

function removekey(tbl, key) local e = tbl[key]; tbl[key] = nil; return e end

local groups = {}
local debugMode = false -- used to show group bounds/anchors and mouse bounds


local windower_settings = windower.get_windower_settings()
local ui_res = {x = windower_settings.ui_x_res, y = windower_settings.ui_y_res}
local ff_res = {x = windower_settings.x_res,    y = windower_settings.y_res}
local base_scalar = ((ui_res.x * 1.0) / (ff_res.x * 1.0))
local ui_anchors = {x = {left = 0, center = ui_res.x / 2, right  = ui_res.x}, y = {top = 0, center = ui_res.y / 2, bottom = ui_res.y}}

local mouse_bounds = { --mitigates sticky-keys on drag-offscreen. otherwise, the user has to release, click, & release again to release the drag once the mouse re-enters windower
	left  = ui_res.x * 0.0025, top   = ui_res.y * 0.004, -- This feature is not foolproof - it can misfire if the mouse moves out of FFXI too rapidly and it can misfire when
	right = ui_res.x * 0.9975, btm   = ui_res.y * 0.996, -- dragging small objects (like single-line text boxes) along the border of FFXI - but overall this should be a net gain, I hope.
}
local mouse_bound_ts = { -- visual reference for mouse_bounds, shown on group.debug_mode(true), hidden on group.debug_mode(false)
	left  = images.new({draggable=false, size = {width  = mouse_bounds.left, height = ui_res.y}, pos = {x = 0,                  y = 0}, color = {red = 0}}),
	right = images.new({draggable=false, size = {width  = mouse_bounds.left, height = ui_res.y}, pos = {x = mouse_bounds.right, y = 0}, color = {red = 0}}),
	top   = images.new({draggable=false, size = {height = mouse_bounds.top,  width  = ui_res.x}, pos = {x = 0,                  y = 0}, color = {red = 0}}),
	btm   = images.new({draggable=false, size = {height = mouse_bounds.top,  width  = ui_res.x}, pos = {x = 0,   y = mouse_bounds.btm}, color = {red = 0}}),
}

local default_setting = T{scalar = base_scalar, anchor_offset = {x = 0, y = 0}}
config = require('config')
rjg_defaults = {scalar = base_scalar, group_data = {}}
rjg_settings = config.load('data/rj_groups_data.xml', rjg_defaults)
config.save(rjg_settings)

local cache = {groups = {}, elements = {}} -- the actual location of w.m and t.m (all other references are metatable pointers to here. those deep tables are actually just traversals of multiple meta pointers)
local rev_z_index = {groups = {}, elements = {}} -- used to iterate elements by z-index (reversed for ipairs usage)
local archive = {} -- original positional data, read-only and used in scalar formulas to protect both the archive and formulas from math decay
local name_lookup = {} -- names don't change on destroy>rebuild

local drag
local click
local hover
local absorbed -- used for absorbing various mouse events that aren't registered for visuals or events
local absorbed_clicks = {} -- used for handing the absorbed clicks to the release handlers

local arrow = {pos = {}} -- used for the hover_arrow visual
local auto_hide = false -- used for hiding groups during log/zone in/out

local event_keys = {
	left_click = true,
	right_click = true,
	middle_click = true,
	scroll = true,
	hover = true,
}

local event_keys_map = {
	click = 'left_click',
}

local visual_keys = {
	left_click_travel = true,
	left_click_toggle = true,
	left_click_bulge = true,
	left_click_color = true,
	left_click_tint = true,
	right_click_travel = true,
	right_click_toggle = true,
	right_click_bulge = true,
	right_click_color = true,
	right_click_tint = true,
	middle_click_travel = true,
	middle_click_toggle = true,
	middle_click_bulge = true,
	middle_click_color = true,
	middle_click_tint = true,
	scroll_zoom = true,
	hover_travel = true,
	hover_bulge = true,
	hover_tint = true,
	hover_arrow = true,
	hover_color = true, 
}

local visual_keys_map = {
	click_travel = 'left_click_travel',
	click_toggle = 'left_click_toggle',
	click_bulge = 'left_click_bulge',
	click_tint = 'left_click_tint',
	zoom = 'scroll_zoom',
	scroll = 'scroll_zoom'
}

local opposing_modes = {
	left = {'right', 'middle'},
	right = {'left', 'middle'},
	middle = {'left', 'right'},
}

local fx = {}
function fx.move_or_grow(m, growth, offset, mod) --fx.move_or_grow(m, {x=1, y=1}, {x=0, y=0}, 1)
	-- PREPARE/UPDATE FX INFO
	m.fxinfo = m.fxinfo or {count=0, growth={x=0, y=0}, offset={x=0, y=0}}
	m.fxinfo.count = m.fxinfo.count + mod
	local mod2 = m.fxinfo.count > 0 and 1 or 0 -- helper to ensure scalars are restored at the end
	m.fxinfo.growth = {x = m.fxinfo.growth.x + (growth.x * m.w.m.scalar * mod), y = m.fxinfo.growth.y + (growth.y * m.w.m.scalar * mod)}
	m.fxinfo.offset = {x = m.fxinfo.offset.x + (offset.x * m.w.m.scalar * mod), y = m.fxinfo.offset.y + (offset.y * m.w.m.scalar * mod)}
	m.fxinfo.size   = {x = m.scalars.size[1]  + (m.fxinfo.growth.x * mod2),     y = m.scalars.size[2]  + (m.fxinfo.growth.y * mod2)    }
	m.fxinfo.pos    = {x = m.scalars.pos.x    + (m.fxinfo.offset.x * mod2),     y = m.scalars.pos.y    + (m.fxinfo.offset.y * mod2)    }
	
	-- APPLY FX TO M
	m.t:size(m.fxinfo.size.x, m.fxinfo.size.y)
	m.t:pos (m.fxinfo.pos.x , m.fxinfo.pos.y )
	
	-- APPLY FX TO EMBED OFFSETS (don't touch sizes since that can produce clipping in texts)
	for _, m2 in pairs(m.embeds) do
		m2.t:pos(m.fxinfo.pos.x + (m2.source.offset.x * m.w.m.scalar) + (m.fxinfo.growth.x / 2), m.fxinfo.pos.y + (m2.source.offset.y * m.w.m.scalar) + (m.fxinfo.growth.y / 2))
	end
	
	-- DISCARD FXINFO AT END OF FX
	m.fxinfo = m.fxinfo.count > 0 and m.fxinfo or nil
end
	
function fx.tint(m, tint, mod) --fx.tint(m, -30, 1)
	-- PREPARE/UPDATE FX INFO
	m.tint_info = m.tint_info or {count=0, tint=0}
	m.tint_info.count = m.tint_info.count + mod
	m.tint_info.tint  = m.tint_info.tint + (tint * mod)
	
	-- PREPARE & APPLY TINTED-COLOR
	local color = {table.unpack(m.color)}
	for i, v in ipairs(color) do
		color[i] = (v + m.tint_info.tint):max(0):min(255)
	end
	local result = m.tint_info.count > 0 and color or m.color
	m.t:color(table.unpack(result))
	
	-- DISCARD TINT_INFO AT END OF FX
	m.tint_info = m.tint_info.count > 0 and m.tint_info or nil
end

function fx.click_toggle(m, new_state)
	m.toggle_state = new_state
	m.color = {table.unpack(m.toggle_colors[new_state])}
	m.t:color(table.unpack(m.toggle_colors[new_state]))
end

local function undo_fx(m)
	if m.fxinfo then fx.move_or_grow(m, {x=0,y=0}, {x=0,y=0}, -m.fxinfo.count) end
	if m.tint_info then fx.tint(m, 0, -m.tint_info.count) end
end

local run_visual = {}
function run_visual.hover(m, hovered, release_override)
	-- IGNORE/QUEUE HOVER/UNHOVER DURING CLICK/DRAG
	if not release_override and (click or drag) then return end
	
	-- MOVE DIAGONALLY ON HOVER/UNHOVER
	if m.visuals.hover.travel then
		fx.move_or_grow(m, {x=0,y=0}, m.visuals.hover.travel, {[true]=1,[false]=-1}[hovered])
	end
	
	-- BULGE/UNBULGE ON HOVER/UNHOVER
	if m.visuals.hover.bulge then
		local offset = {x = -(m.visuals.hover.bulge.x / 2), y = -(m.visuals.hover.bulge.y / 2)}
		fx.move_or_grow(m, m.visuals.hover.bulge, offset, {[true]=1,[false]=-1}[hovered])
	end
	
	-- DARKEN ON HOVER
	if m.visuals.hover.tint then
		fx.tint(m, m.visuals.hover.tint, {[true]=1,[false]=-1}[hovered])
	end
	
	-- SHOW ARROW ON HOVER
	if m.visuals.hover.arrow then
		if hovered then
			if arrow.t then arrow.t:destroy() end
			arrow.t = images.new('Arrow',{draggable=false})
			arrow.t:path(windower.addon_path .. 'data/Arrow001.png')
			arrow.t:fit(false)
			arrow.size_1, arrow.size_2 = (m.scalars.size[2] * 0.66) * 1.66, m.scalars.size[2] * 0.66
			arrow.t:size(arrow.size_1, arrow.size_2)
			arrow.pos_x, arrow.pos_y = m.scalars.pos.x - arrow.size_1 * 0.9, m.scalars.pos.y + (m.scalars.size[2] - arrow.size_2) / 2
			arrow.t:pos(arrow.pos_x, arrow.pos_y)
			arrow.t:show()
		elseif arrow.t then
			arrow.t:destroy()
			arrow.t = nil
		end
	end
	
	-- RECOLOR ON HOVER (overrides hover tint)
	if m.visuals.hover.color then
		local color = hovered and {m.visuals.hover.color()} or m.color or {m.t:color()}
		m.t:color(color[1], color[2], color[3])
	end
end
	
function run_visual.click(m, effects, release)
	-- MOVE DIAGONALLY ON CLICK/RELEASE
	if effects.click_travel then
		fx.move_or_grow(m, {x=0,y=0}, effects.click_travel, {[true]=-1,[false]=1}[release])
	end
	
	-- BULGE/UNBULGE ON CLICK/RELEASE
	if effects.click_bulge then
		local offset = {x = -(effects.click_bulge.x / 2), y = -(effects.click_bulge.y / 2)}
		fx.move_or_grow(m, effects.click_bulge, offset, {[true]=-1,[false]=1}[release])
	end
	
	-- CHANGE COLOR ON TOGGLE
	if effects.click_toggle and release and not click.cancel and not m.disabled then
		fx.click_toggle(m, not m.toggle_state)
	end
	
	-- TINT DURING CLICK DOWN
	if effects.click_tint then
		fx.tint(m, effects.click_tint, {[true]=-1,[false]=1}[release])
	end
	
	-- RECOLOR DURING CLICK DOWN
	if effects.click_color then
		local color = not release and {effects.click_color()} or m.color or {m.t:color()}
		m.t:color(color[1], color[2], color[3])
	end
end
	
function run_visual.scroll(m, effect, mode, mouse)
	-- PREVENT PREMATURE SCROLLS (Scrolls faster than the 0.1s bounds update) (delayed for text extents)
	if m.w.m.master.m.awaiting_bounds then return --[[log('awaiting bounds update')]] end
	
	local wm, delta = m.w.m.master.m, m.w.m.master.m.scalar * (effect.percent * {up=1,down=-1}[mode]) / 100
	local new_scalar, change = (wm.scalar + delta), delta / wm.scalar
	if new_scalar < wm.scale_limit.min or new_scalar > wm.scale_limit.max then return --[[log('min/max scalar limit met')]] end
	local zoom_offset = (function()
		local growth = {x = wm.sync.bounds.width * change, y = wm.sync.bounds.height * change}
		if effect.focus == 'center' then
			local offset_x = growth.x * {left = -0.5, center = 1, right  = 0.5}[wm.horizontal_align]
			local offset_y = growth.y * {top  = -0.5, center = 1, bottom = 0.5}[wm.vertical_align]
			return {x = offset_x, y = offset_y}
		elseif effect.focus == 'cursor' then
			local mouse_ratio = {x = (mouse.x - wm.sync.bounds.left) / wm.sync.bounds.width, y = (mouse.y - wm.sync.bounds.top) / wm.sync.bounds.height}
			local h_offset = growth.x * {left = 0, center = 0.5, right  = 1}[wm.horizontal_align]
			local v_offset = growth.y * {top  = 0, center = 0.5, bottom = 1}[wm.vertical_align]
			return {x = wm.sync.bounds.width * -change * mouse_ratio.x + h_offset, y = wm.sync.bounds.height * -change * mouse_ratio.y + v_offset}
		else
			--do nothing, group will stay anchored to top-left and grow rightwards and downwards
		end
	end)()
	wm.w:scalar(new_scalar, zoom_offset)
end
run_visual.left = run_visual.click
run_visual.right = run_visual.click
run_visual.middle = run_visual.click

local function update_element_scalars(m, zoom_offset, save_offset)
	local wm, save_offset, zoom_offset = m.w.m.master.m, save_offset or m.w.m.master.m.setting.anchor_offset, zoom_offset or {x = 0, y = 0}
	undo_fx(m) -- Undo fx, otherwise fx counts will never return to 0 and effects will linger
	m.scalars.pos.x = wm.sync.bounds.archive.anchor.x + (m.archive.offset.x * wm.scalar) + zoom_offset.x + save_offset.x
	m.scalars.pos.y = wm.sync.bounds.archive.anchor.y + (m.archive.offset.y * wm.scalar) + zoom_offset.y + save_offset.y
	m.scalars.size = {m.archive.size[1] * wm.scalar, m.archive.size[2] * wm.scalar}
	m.t:pos(m.scalars.pos.x, m.scalars.pos.y)
	m.t:size(m.scalars.size[1], m.scalars.size[2])
	if m.pad then m.t:pad(m.pad * wm.scalar) end
	if m.stroke_width then m.t:stroke_width(m.stroke_width * wm.scalar) end
end

local function build_element_archive(w)
	if w ~= w.m.master then return error('A non-master-group was passed into build_element_archive') end
	
	-- BUILD ARCHIVES
	for _, w2 in ipairs(w.m.sync.groups) do
		for _, t in ipairs(w2.m.elements) do
			local extents = {t[t.get_extents and 'get_extents' or 'extents'](t)}
			extents = t.m.class == 'images' and extents or {extents[1] + t.m.archive.pos.x, extents[2] + t.m.archive.pos.y}
			t.m.archive.ext = {x = extents[1], y = extents[2]}
		end
	end
	
	-- BUILD BOUNDS ARCHIVES
	local b_archive = w.m.sync.bounds.archive
	for _, w2 in ipairs(w.m.sync.groups) do
		for _, t in ipairs(w2.m.elements) do
			b_archive.top   = b_archive.top   == nil and t.m.archive.pos.y or b_archive.top   :min(t.m.archive.pos.y)
			b_archive.left  = b_archive.left  == nil and t.m.archive.pos.x or b_archive.left  :min(t.m.archive.pos.x)
			b_archive.btm   = b_archive.btm   == nil and t.m.archive.ext.y or b_archive.btm   :max(t.m.archive.ext.y)
			b_archive.right = b_archive.right == nil and t.m.archive.ext.x or b_archive.right :max(t.m.archive.ext.x)
		end
	end
	b_archive.width, b_archive.height = b_archive.right - b_archive.left, b_archive.btm - b_archive.top
	
	-- GET INITIAL & FINAL ANCHOR & THE OFFSET OF THE TWO
	local init_anchor_x = {left = b_archive.left, center = b_archive.left + b_archive.width  / 2, right  = b_archive.right}[w.m.horizontal_align]
	local init_anchor_y = {top  = b_archive.top,  center = b_archive.top  + b_archive.height / 2, bottom = b_archive.btm}  [w.m.vertical_align]
	b_archive.anchor = {x = ui_anchors.x[w.m.horizontal_align] + (w.m.offset.x * w.m.scalar), y = ui_anchors.y[w.m.vertical_align] + (w.m.offset.y * w.m.scalar)}
	local anchor_offset = {x = b_archive.anchor.x - init_anchor_x, y = b_archive.anchor.y - init_anchor_y}
	
	-- BUILD ELEMENT ARCHIVE OFFSETS (these make the elements grow out from their group's anchor)
	for _, w2 in ipairs(w.m.sync.groups) do
		for _, t in ipairs(w2.m.elements) do
			t.m.archive.pos = {x = t.m.archive.pos.x + anchor_offset.x, y = t.m.archive.pos.y + anchor_offset.y}
			t.m.archive.offset = {x = t.m.archive.pos.x - b_archive.anchor.x, y = t.m.archive.pos.y - b_archive.anchor.y}
		end
	end
	
	-- BUILD/SET SCALARS & THEN REVEAL THE ELEMENTS
	for _, w2 in ipairs(w.m.sync.groups) do
		for _, t in ipairs(w2.m.elements) do
			update_element_scalars(t.m)
			if w2.m.visible then -- NOTEWORTHY: We've replaced :visible with :transparency due to native limitations of text:extents of visible=false texts... /shrug
				t:transparency(t.m.transparency)
				if t.m.class == 'texts' then
					t:bg_transparency(t.m.bg_transparency)
					t:stroke_transparency(t.m.stroke_transparency)
				end
			end
		end
	end
end

local function queue_group_data_collection(w, incoming_seed)
	if w.m.master ~= w then return error('only queue data collection for master groups') end
	w.m.init_seed = w.m.init_seed + 1 
	coroutine.schedule(function()
		if incoming_seed ~= w.m.init_seed then return end -- re-queue to start 0.1s after the last-queued element is added
		build_element_archive(w) -- gets archive, bounds archive, archive offsets, and initial scalars
		w.get_bounds:schedule(0.1, w) --update bounds after build_element_archive implements scalars
		w.m.init_seed = nil --mark 
	end, 0.1 + (incoming_seed / 1000)) -- increment schedule by element's position in the queue to delay the final call more than the initial calls
end

local function get_synced_elements(w)
	local elements = T{}
	for _, w2 in pairs(w.m.master.m.sync.groups) do
		elements:extend(w2.m.elements)
	end
	return elements
end

local function transparency_hover(t, x, y) -- required due to us overriding visibility with transparency due to limitations of text:extents of visible=false text.
	if not t.m.visible or t.w.m.auto_hidden then
		return false
	end
	-- IMPORTANT: t:extents of visible texts returns initial values when called from here. There is a scope issue of
	if t.m.scalars.ext == nil then return end --ignore errors inbetween init/zoom and get_bounds
	return (t.m.scalars.pos.x <= x and x <= t.m.scalars.ext.x -- sorts from the rawset override. The scope issue is
		or t.m.scalars.pos.x >= x and x >= t.m.scalars.ext.x) -- not affecting us, however, due to having access to
	and (t.m.scalars.pos.y <= y and y <= t.m.scalars.ext.y    -- up-to-date extents from our local t.m.scalars.
		or t.m.scalars.pos.y >= y and y >= t.m.scalars.ext.y)
end

groups.new = function(str, w_defaults, master)
	if type(str) ~= 'string' then
		return error('Provide a name for groups')
	elseif w_defaults and w_defaults.horizontal_align and not {left=true, center=true, right=true}[w_defaults.horizontal_align] then
		return error('Provide "%s" a valid horizontal_align value: left, center, or right':format(str))
	elseif w_defaults and w_defaults.vertical_align and not {top=true, center=true, bottom=true}[w_defaults.vertical_align] then
		return error('Provide "%s" a valid vertical_align value: top, center, or bottom':format(str))
	end
	
	local w = name_lookup[str]
	-- NEW group: CREATE & RETURN
	if w == nil then
		local m        = w_defaults or {}
		w              = {}
		w.m            = setmetatable({}, {__index = m, __newindex = m, __call = function() return m end}) --non-recursive shortcut
		w.m.class      = 'group'
		w.name         = str
		m.name         = str
		m.w            = w
		m.s_name       = w.name:gsub('%s+', '_'):lower() --for settings, where no spaces are allowed
		m.master       = master or w
		m.visible      = m.visible or false -- default invisible
		m.transparency = m.transparency or m.visible and 0 or 1 -- default transparent
		m.elements     = {}
		m.auto_hide    = (function() if m.auto_hide ~= nil then return m.auto_hide else return true end end)()
		
		if w == m.master then
			-- MASTER group
			rjg_settings.group_data[m.s_name] = rjg_settings.group_data[m.s_name] or default_setting:copy()
			m.setting         = rjg_settings.group_data[m.s_name]
			config.save(rjg_settings)
			m.scalar           = m.setting.scalar
			m.horizontal_align = m.horizontal_align or 'left'
			m.vertical_align   = m.vertical_align   or 'top'
			m.offset           = m.offset or {x = 0, y = 0}
			m.scale_limit      = m.scale_limit or {min = 0.1, max = 99}
			m.scale_limit      = {min = m.scale_limit.min * base_scalar, max = m.scale_limit.max * base_scalar}
			m.sync             = {groups = {w}, bounds = {archive = {}, t = images.new({draggable=false})}, anchor = {pos = {}}}
			m.init_seed        = 0 -- used to collect bounds after last element added
			m.awaiting_bounds  = true -- used to detect end of bounds collection
		else
			-- SYNCED group
			if m.offset then error('syncee groups do not support offsets, only master groups do') end
			table.insert(m.master.m.sync.groups, w)
		end
		
		table.insert(rev_z_index.groups, 1, w) -- cache for z-index iteration
		cache.groups[w] = m -- cache for lookup by w
		name_lookup[str] = w -- cache for lookup by name (doesn't change on rebuild)
		return setmetatable(w, {__index = groups}) -- redirect to master class
		
	-- CACHED group: RETURN CACHE
	else
		return w, cache.groups[w]
	end
end

function groups.new_synced_group(source_w, str, w_defaults)
	-- CREATE OR RETURN ELEMENT
	local emb_w = groups.new(str, w_defaults, source_w)
	return emb_w
end

function groups.new_element(w, str, m_defaults, settings2, root_settings)
	-- ENSURE PROPER VARS
	if type(m_defaults) ~= 'table' then return error('provide a m_defaults table') end
	if _G[m_defaults.class] == nil then return error('invalid class in m_defaults') end
	if type(m_defaults.name) ~= 'string' then return error('provide a name for group elements') end
	if m_defaults.size == nil then return error('provide a m_defaults.size for group elements') end
	if m_defaults.pos == nil then return error('provide a m.defaults.pos for group elements') end
	
	-- FIND EXISTING CACHE
	local t = name_lookup[w.name .. '_' .. m_defaults.class .. '_' .. m_defaults.name]
	
	-- NEW ELEMENT: CREATE & RETURN
	if t == nil then
		local t, m, wm = _G[m_defaults.class].new(str, settings2, root_settings), m_defaults, cache.groups[w]
		rawset(t, 'm', setmetatable({}, {__index = m, __newindex = m, __call = function() return m end})) --add pointer to m
		rawset(t, 'w', setmetatable({}, {__index = w, __newindex = w, __call = function() return w end})) --add pointer to w
		rawset(t, 'hover', transparency_hover) --this has some scope issues around extents, but it works with our scalars
		
		-- SETUP META
		m.w               = w
		m.t               = t
		m.events          = {}
		m.embeds          = {}
		m.draggable       = m.draggable or {}
		m.drag_tolerance  = m.drag_tolerance or 0
		m.disabled        = (function() if m.disabled ~= nil then return m.disabled else return false end end)()
		m.visuals         = {}
		m.color           = m.color or {t:color()}
		m.visible         = (function() if m.visible ~= nil then return m.visible else return wm.visible end end)() --default to visibility or wm.visibility
		m.transparency    = (function() if m.transparency ~= nil then return m.transparency else return 0 end end)() -- default to opaque (not transparent)
		if m.class == 'texts' then
			m.bg_visible      = (function() if m.bg_visible ~= nil then return m.bg_visible else return true end end)() -- default visible
			m.bg_transparency = (function() if m.bg_transparency ~= nil then return m.bg_transparency else return m.bg_visible and 0 or 1 end end)() -- default opaque
			m.stroke_transparency = (function() if m.stroke_transparency then return m.stroke_transparency else return m.visible and 0 or 1 end end)() -- default opaque
		end
		
		-- INITIALIZE POSITIONAL ARCHIVE
		archive[t], m.size = {}, {images = m.size, texts = {m.size, 0}}[m.class]
		m.archive = setmetatable({}, {__index = archive[t], __newindex = archive[t], __call = function() return archive[t] end})
		archive[t].pos, archive[t].size = removekey(m, 'pos'), removekey(m, 'size')
		t:pos(archive[t].pos.x, archive[t].pos.y) -- set this to prepare for initial extents gathering
		t:size(archive[t].size[1], archive[t].size[2]) -- set this to prepare for initial extents gathering
		
		-- QUEUE group DATA COLLECTION (text extents require this delay)
		m.scalars = {pos = {}}
		wm.init_seed = wm.init_seed or 0 -- re-inialize init_seed for later additions
		queue_group_data_collection(wm.master, wm.master.m.init_seed + 1) --delay this to give the initial text extents time to populate

		-- STANDARD T MODIFICATIONS
		t:draggable(false)
		t:color(table.unpack(m.color))
		t:visible(true)
		t:transparency(1) -- queue_group_data_collection will reveal the element, if applicable, once it executes
		if m.class == 'texts' then 
			t:bg_transparency(1)
			t:stroke_transparency(1)
		end
		
		-- CACHE META
		table.insert(wm.elements, 1, t)
		table.insert(rev_z_index.elements, 1, t) -- cache for z-index based iteration
		cache.elements[t] = m -- cache for lookup by t
		name_lookup[w.name .. '_' .. m.class .. '_' .. m.name] = t -- cache for lookup by name (doesn't change on rebuild)
		
		return t, m
		
	-- CACHED ELEMENT: RETURN
	else
		return t, cache.elements[t]
	end
end

function groups.new_embedded_element(w, source_t, str, embed_defaults, settings2, root_settings)
	-- CREATE OR RETURN ELEMENT
	local src_m, emb_t, emb_m  = cache.elements[source_t], w:new_element(str, embed_defaults, settings2, root_settings)
	
	-- STORE INTERNAL OFFSET (relative so it is relevant regardless of scalars)
	emb_m.source = {offset = {x = archive[emb_t].pos.x - archive[src_m.t].pos.x, y = archive[emb_t].pos.y - archive[src_m.t].pos.y}}
	emb_m.source.m = setmetatable({}, {__index = src_m, __newindex = src_m, __call = function() return src_m end})
	
	-- STORE ELEMENT POINTER IN SOURCE
	src_m.embeds[embed_defaults.name] = setmetatable({}, {__index = emb_m, __newindex = emb_m, __call = function() return emb_m end})
	
	return emb_t, emb_m
end

function groups.get_bounds(w)
	--GET group SYNC BOUNDS
	local wm, bounds, anchor, archive = w.m.master.m, w.m.master.m.sync.bounds, w.m.master.m.sync.anchor, w.m.master.m.sync.bounds.archive
	bounds.top, bounds.left, bounds.btm, bounds.right, archive.top, archive.left = nil, nil, nil, nil, nil, nil
	for _, t in pairs(get_synced_elements(w)) do --not iterating
		-- CALCULATE STANDARDIZED EXTENTS (images and texts report them differently, unforuntately)
		local extent = {t[t.get_extents and 'get_extents' or 'extents'](t)} -- method terminology is inconsistent
		extent = t.m.class == 'images' and extent or {t.m.scalars.pos.x + extent[1], t.m.scalars.pos.y + extent[2]} --images: pos+size, texts: pos
		t.m.scalars.ext = {x = extent[1], y = extent[2]}
		
		-- INCREMENT SCALAED T/L/B/R BOUNDS
		bounds.top   = bounds.top   == nil and t.m.scalars.pos.y or bounds.top   :min(t.m.scalars.pos.y)
		bounds.left  = bounds.left  == nil and t.m.scalars.pos.x or bounds.left  :min(t.m.scalars.pos.x)
		bounds.btm   = bounds.btm   == nil and t.m.scalars.ext.y or bounds.btm   :max(t.m.scalars.ext.y)
		bounds.right = bounds.right == nil and t.m.scalars.ext.x or bounds.right :max(t.m.scalars.ext.x)
	end
	-- UPDATE FINAL BOUNDS
	bounds.width, bounds.height = bounds.right - bounds.left, bounds.btm - bounds.top
	bounds.pos, bounds.size = {x = bounds.left, y = bounds.top}, {bounds.width, bounds.height}
	bounds.t:pos(bounds.pos.x, bounds.pos.y)
	bounds.t:size(bounds.size[1], bounds.size[2])
	
	-- UPDATE FINAL ANCHOR
	if anchor.t then anchor.t:destroy() end
	anchor.t = images.new({draggable = false})
	anchor.pos.x  = {left = bounds.left, center = bounds.left + (bounds.width  / 2), right  = bounds.right}[wm.horizontal_align]
	anchor.pos.y  = {top  = bounds.top,  center = bounds.top  + (bounds.height / 2), bottom = bounds.btm  }[wm.vertical_align]
	anchor.size_1 = {left = 4 * wm.scalar, center = 4 * wm.scalar, right  = -4 * wm.scalar}[wm.horizontal_align]
	anchor.size_2 = {top  = 4 * wm.scalar, center = 4 * wm.scalar, bottom = -4 * wm.scalar}[wm.vertical_align]
	anchor.t:pos(anchor.pos.x, anchor.pos.y)
	anchor.t:size(anchor.size_1, anchor.size_2)
	
	-- SAVE UPDATED ANCHOR OFFSET
	w.m.setting.anchor_offset = {x = anchor.pos.x - archive.anchor.x, y = anchor.pos.y -archive.anchor.y}
	config.save(rjg_settings)
	
	w:show_bounds(debugMode)
	w.m.awaiting_bounds = nil
	
	-- RESCUE OFFSCREEN groupS
	if bounds.right < 0 or bounds.btm < 0 or bounds.left > ui_res.x or bounds.top > ui_res.y then
		wm.w:reset_settings()
	end
end

function groups.show_bounds(w, bool)
	if bool == nil then
		return w.m.master.m.sync.bounds.t:visible()
	end
	
	w.m.master.m.sync.bounds.t:visible(bool)
	w.m.master.m.sync.bounds.t:color(0,255,255)
	w.m.master.m.sync.bounds.t:transparency(0.9)
	w.m.master.m.sync.anchor.t:visible(bool)
	w.m.master.m.sync.anchor.t:color(255,0,255)
	w.m.master.m.sync.anchor.t:transparency(0.5)
end

function groups.call_drag_sync(data)
	local wm = drag.m.w.m.master.m --float reference up to master group
	if (data or {}).release then
		-- END OF DRAG: GET NEW BOUNDS
		wm.w:get_bounds() -- this will also save new position (can't save till new bound's offset is calculated)
		
		for _, t in pairs(drag.synced_elements) do
			if not hover or t.m() ~= hover.m then
				--undo_fx(t.m) --otherwise, fx can linger and accumulate on drags
			end
		end
	else
		-- ENFORCE DRAG BOUNDS/UPDATE BOUNDS ANCHOR
		local drag_bounds, group_bounds = wm.drag_bounds, wm.sync.bounds
		local limits = drag_bounds and {
			top    = 0 - group_bounds.pos.y + (drag_bounds.top  * wm.scalar),
			left   = 0 - group_bounds.pos.x + (drag_bounds.left * wm.scalar),
			bottom = ui_res.y -  group_bounds.pos.y - group_bounds.height - (drag_bounds.bottom * wm.scalar),
			right  = ui_res.x - (group_bounds.pos.x + group_bounds.width) - (drag_bounds.right  * wm.scalar),
		}
		drag.delta = limits and {x = drag.delta.x:min(limits.right):max(limits.left), y = drag.delta.y:min(limits.bottom):max(limits.top)} or drag.delta
		if drag.delta.x == 0 and drag.delta.y == 0 then return --[[log('delta is 0')]] end
		
		-- MID DRAG: MOVE ELEMENTS, ANCHOR, & ARROW IN UNISON
		for _, t in pairs(drag.synced_elements) do
			t.m.scalars.pos.x, t.m.scalars.pos.y = t.m.scalars.pos.x + drag.delta.x, t.m.scalars.pos.y + drag.delta.y
			t.m.scalars.ext.x, t.m.scalars.ext.y = t.m.scalars.ext.x + drag.delta.x, t.m.scalars.ext.y + drag.delta.y
			t.m.t:pos(t.m.scalars.pos.x, t.m.scalars.pos.y)
		end
		if wm.sync then
			wm.sync.bounds.pos = {x = wm.sync.bounds.pos.x + drag.delta.x, y = wm.sync.bounds.pos.y + drag.delta.y}
			wm.sync.bounds.t:pos(wm.sync.bounds.pos.x, wm.sync.bounds.pos.y)
			wm.sync.anchor.pos = {x = wm.sync.anchor.pos.x + drag.delta.x, y = wm.sync.anchor.pos.y + drag.delta.y}
			wm.sync.anchor.t:pos(wm.sync.anchor.pos.x, wm.sync.anchor.pos.y)
		end
		if arrow.t then
			arrow.pos_x, arrow.pos_y = arrow.pos_x + drag.delta.x, arrow.pos_y + drag.delta.y
			arrow.t:pos(arrow.pos_x, arrow.pos_y)
		end
	end
end

function groups.name_lookup(str)
	return name_lookup[str]
end

function groups.pos(w, x, y)
	if x == nil then
		return w.m.master.m.sync.bounds.left, w.m.master.m.sync.bounds.top
	end

	local wm, setting = w.m.master.m, rjg_settings.group_data[w.m.master.m.s_name]
	local x, y = x + wm.e1.archive.offset_x * wm.scalar, y + wm.e1.archive.offset_y * wm.scalar -- factor for e1 since that's used on load
	setting.anchor_offset.x, setting.anchor_offset.y = x, y
	config.save(rjg_settings)
	w.m.master:scalar(w.m.master.m.scalar) -- this just so happens to reposition everything using the modified setting
end

function groups.scalar(w, new_scalar, zoom_offset)
	if new_scalar == nil then
		return w.m.master.m.scalar
	end

	-- GET MASTER REFERENCE & ZOOM/SAVE OFFSETS
	local wm, setting, zoom_offset = w.m.master.m, rjg_settings.group_data[w.m.master.m.s_name], zoom_offset or {x = 0, y = 0}
	if wm.scale_limit and new_scalar < wm.scale_limit.min or new_scalar > wm.scale_limit.max then return --[[log('min/max scalar limit met')]] end
	
	-- ENFORCE DRAG BOUNDS ON GROWTH
	if wm.drag_bounds and new_scalar > wm.scalar then
		local change, anchor, bounds, new_bounds = (new_scalar - wm.scalar) / wm.scalar, wm.sync.anchor, wm.sync.bounds, {}
		local growth = {x = wm.sync.bounds.width * change, y = wm.sync.bounds.height * change}
		if bounds.width + growth.x > ui_res.x or bounds.height + growth.y > ui_res.y then return --[[log('ui res exceeded on a drag_bounds group')]] end
		local new_anchor = {x = anchor.pos.x + zoom_offset.x, y = anchor.pos.y + zoom_offset.y}
		new_bounds.left = new_anchor.x + ((bounds.width  + growth.x) * {left = 0, center = -0.5, right  = -1}[wm.horizontal_align])
		new_bounds.top =  new_anchor.y + ((bounds.height + growth.y) * {top  = 0, center = -0.5, bottom = -1}[wm.vertical_align])
		new_bounds.right, new_bounds.btm = new_bounds.left + bounds.width + growth.x, new_bounds.top + bounds.height + growth.y
		local excess = {left = new_bounds.left:min(0), top = new_bounds.top:min(0), right = (ui_res.x - new_bounds.right):min(0), btm = (ui_res.y - new_bounds.btm):min(0)}
		zoom_offset = {x = zoom_offset.x - excess.left + excess.right, y = zoom_offset.y - excess.top + excess.btm}
	end
	
	-- UPDATE SYNCED-ELEMENT
	wm.scalar = new_scalar --Must be done after compensating e1>anchor change, or else it adds pos creep
	for _, t in pairs(get_synced_elements(w)) do
		update_element_scalars(t.m, zoom_offset)
	end
	
	-- UPDATE BOUNDS
	wm.awaiting_bounds = true
	wm.setting.scalar = new_scalar
	w.get_bounds:schedule(0.1, wm.w) --will save new setting
	
	-- UPDATE ARROW
	if arrow.t then
		if hover then run_visual.hover(hover.m, true) end
	end
end

function groups.visible(w, bool, auto_hide, propagate) --NOTE: visibility is being redirected to transparency due to texts.extents failing on invisible, but not transparent, text
	if bool == nil then
		return w.m.visible
	end
	
	w.m.visible = (function() if auto_hide then return w.m.visible else return bool end end)()
	local subset = (w == w.m.master and propagate) and w.m.sync.groups or {w}
	for _, w2 in ipairs(subset) do
		if not bool or w2.m.visible then --always cascade hide command, only cascade show to should-be-visible groups
			w2.m.visible = (function() if auto_hide then return w2.m.visible else return bool end end)()
			for _, t in ipairs(w2.m.elements) do
				t.m.visible = (function() if auto_hide then return t.m.visible else return bool end end)()
				t:transparency(bool and t.m.transparency or 1)
				if t.m.class == 'texts' then
					t:bg_transparency(bool and t.m.bg_transparency or 1)
					t:stroke_transparency(bool and t.m.stroke_transparency or 1)
				end
			end
		end
	end
end

function groups.empty(w, destroy) --caution, this could move your anchor. hiding the group may be better.
	-- ADD SELF TO QUEUE
	local queued_groups = {w}
	
	-- CHILD group: BREAK LINK (DESTROY ONLY)
	if destroy and w.m.master ~= w then
		table.remove(w.m.master.m.sync, T(w.m.master.m.sync):find(w))
	
	-- MASTER group: PUSH SYNC TO QUEUE (DESTROY ONLY)
	elseif destroy and w.m.sync then
		queued_groups = w.m.sync.groups
	end
	
	-- EMPTY OR DESTROY QUEUED groupS
	for _, w2 in ipairs(groups) do
		for _, t in ipairs(w2.m.elements) do
			w2:destroy_element(t)
		end
		if destroy then
			name_lookup[w2.name] = nil
			table.remove(rev_z_index.groups, T(rev_z_index.groups):find(w2))
			w.m.master.m.sync.bounds.t:destroy()
			w.m.master.m.sync.anchor.t:destroy()
			cache.groups[w2] = nil
			w2 = nil
		end
	end
	
	-- UPDATE MASTER'S BOUNDS
	w.m.master.get_bounds:schedule(0.15, w.m.master)
end

function groups.destroy(w) --caution, this could move your anchor. hiding the group may be better.
	w:empty(true)
end

function groups.destroy_element(w, t)
	name_lookup[w.name .. '_' .. t.m.class .. '_' .. t.m.name] = nil
	table.remove(rev_z_index.elements, T(rev_z_index.elements):find(t))
	cache.elements[t] = nil
	t:destroy()
	t = nil
end

function groups.reset_settings(w)
	w.m.master.m.setting:update(default_setting)
	config.save(rjg_settings)
	w:scalar(w.m.master.m.scalar)
end

function groups.debug_mode(bool)
	if bool == nil then
		return debugMode
	end
	
	debugMode = bool
	mouse_bound_ts.left:visible(bool)
	mouse_bound_ts.top:visible(bool)
	mouse_bound_ts.right:visible(bool)
	mouse_bound_ts.btm:visible(bool)
end

local call_events = function(m, event_mode, event_type, ...)
	if not m then
		return error('A meta object must be provided to call_events for group elements')
	elseif not m.events[event_mode] or not m.events[event_mode][event_type] then
		return error('"%s" has no "%s" event for call_events':format(m.name, event_mode..'_'..event_type))
	end

	for _, func in ipairs(m.events[event_mode][event_type]) do
		func(m, ...)
	end
end

local mouse_processor = {}

function mouse_processor.hover(x, y, delta)
	if click or drag then return end
	for _, t in ipairs(rev_z_index.elements) do
		local m = cache.elements[t]
		if t:hover(x, y) and (m.visuals.hover) then
			absorbed = true
			-- HOVER TRANSITION
			if hover and hover.m.t ~= t then
				run_visual.hover(hover.m, false)
				run_visual.hover(m, true)
				hover.m = m
			-- NEW HOVER
			elseif not hover then
				run_visual.hover(m, true)
				hover = {m = m}
			end
			break
		end
	end
	-- UNHOVER
	if hover and not absorbed then
		run_visual.hover(hover.m, false)
		hover = nil
	end
end

function mouse_processor.drag(x, y, delta)
	if drag then --not destroyed
		absorbed = true
		-- WAIT UNTIL TOLERANCE MET (DEFAULT 0)
		if not drag.active then
			drag.active = (math.abs(x - drag.last_m_pos.x) + math.abs(y - drag.last_m_pos.y)) / 2 > drag.m.drag_tolerance
		end
		-- DRAG IF TOLERANCE MET
		if drag.active then
			drag.delta = {x = x - drag.last_m_pos.x, y = y - drag.last_m_pos.y}
			groups.call_drag_sync()
			drag.last_m_pos = {x = x, y = y}
		end
	end
	
	-- MITIGATE STICKY MOUSE CLICKS (dragging out of bounds while clicked/dragging)
	if (click or drag) and (x < mouse_bounds.left or x > mouse_bounds.right or y < mouse_bounds.top or y > mouse_bounds.btm) then
		mouse_processor.release((click or drag).mode, x, y, delta) -- this removes it, yet
		ignore_following_release = true
	end
end

function mouse_processor.click(mode, x, y, delta)
	-- EMBEDDED CLICKS (ignore these. ex: L-down > *R-down* > R-up > L-up)
	if click or drag then --also ignore clicks until the ignored mode is released
		absorbed = true
	-- STANDALONE CLICKS
	else
		ignore_following_release = nil -- drop this flag on any click-down
		for _, t in ipairs(rev_z_index.elements) do
			if click and drag then break end
			if t:hover(x, y) then
				absorbed = true
				absorbed_clicks[mode] = true
				local m = cache.elements[t]
				if m.visuals[mode] or m.events[mode] then
					click = {m = m, mode = mode, x = x, y = y, effects = m.visuals[mode]}
					-- RUN VISUALS
					if m.visuals[mode] --[[and not m.disabled]] then
						run_visual[mode](m, click.effects, false)
					end
					-- RUN EVENTS
					if m.events[mode] then
						call_events(m, mode, 'click', false)
					end
				end
				if not drag and m.draggable[mode] then
					drag = T{m=m, mode=mode, active=false, last_m_pos={x = x, y = y}, synced_elements=get_synced_elements(m.w)}
				end
			end
		end
	end
end

function mouse_processor.release(mode, x, y, delta)
	-- EMBEDDED RELEASE (ignore these. ex: L-down > R-down > *R-up* > L-up)
	if (click and click.mode ~= mode) or (drag and drag.mode ~= mode) or ignore_following_release then
		absorbed = true
		ignore_following_release = nil
	-- CURRENT RELEASE
	elseif click or drag or absorbed or absorbed_clicks[mode] then
		absorbed = true
		if drag and drag.active then
			groups.call_drag_sync({release=true})
		end
		if click then
			click.release, click.cancel = true, not click.m.t:hover(x, y) or (drag and drag.active)
			-- ON-RELEASE VISUALS (run before events to update toggle vars)
			if click.effects then
				run_visual[click.mode](click.m, click.effects, true)
			end
			-- ON-RELEASE EVENTS
			if not click.cancel and click.m.events[mode] and click.m.t:hover(x, y) then
				call_events(click.m, mode, 'click', true)
			end
		end
		click = nil
		drag = nil
		mouse_processor.hover(x, y, delta) --revive ignored hover/unhovers on release (after nilling click/drag)
		absorbed_clicks[mode] = nil
	end
end

function mouse_processor.scroll(x, y, delta)
	local mode = delta > 0 and 'up' or 'down' --variable delta
	for _, t in ipairs(rev_z_index.elements) do
		if t:hover(x, y) then
			absorbed = true
			local m = cache.elements[t]
			-- RUN EVENT FUNCTIONS
			if m.events.scroll then
				call_events(m, 'scroll', '', mode)
				return -- stop before visual if there is an event (otherwise you could zoom when changing a value.. would seem odd)
			-- RUN VISUAL EFFECTS
			elseif m.visuals.scroll then
				run_visual.scroll(m, m.visuals.scroll.zoom, mode, {x=x, y=y})
				mouse_processor.hover(x, y , delta) --hover won't fire, on its own, without movement
				return -- stop at top-most element
			end
		end
	end
end

local type_maps = {
	[ 0] = {id =  0, type = 'movement'},
	[ 1] = {id =  1, type = 'click',   mode = 'left'},
	[ 2] = {id =  2, type = 'release', mode = 'left'},
	[ 3] = {id =  3, type = '?'}, --my mouse can't trip this
	[ 4] = {id =  4, type = 'click',   mode = 'right'},
	[ 5] = {id =  5, type = 'release', mode = 'right'},
	[ 6] = {id =  6, type = '?'}, --my mouse can't trip this
	[ 7] = {id =  7, type = 'click',   mode = 'middle'},
	[ 8] = {id =  8, type = 'release', mode = 'middle'},
	[ 9] = {id =  9, type = '?'}, --my mouse can't trip this
	[10] = {id = 10, type = 'scroll'},
}

windower.register_event('mouse', function(type, x, y, delta, blocked)
	if blocked then return end
	local type_map = type_maps[type]
	absorbed = false
	
	if type_map.type == 'movement' then
		mouse_processor.hover(x, y, delta)
		mouse_processor.drag (x, y, delta)

	elseif type_map.type == 'click' then
		mouse_processor.click(type_map.mode, x, y, delta)

	elseif type_map.type == 'release' then
		mouse_processor.release(type_map.mode, x, y, delta)
	
	elseif type_map.type == 'scroll' then
		mouse_processor.scroll(x, y, delta)
	end

	return absorbed
end)

function groups.register_visual(t, ...)
	if t.m.class ~= 'images' and t.m.class ~= 'texts' then
		return error('An image or text must be the first argument of register_visual.')
	end
	for _, data in ipairs({...}) do
		local key = visual_keys_map[data[1]] or data[1]
		local key_parts = key:split('_')
		local effect_mode, effect_type = key_parts:remove(1), key_parts:concat('_')
		if not visual_keys[key] then
			error('Visual effect "%s" is not available for group elements.':format(key))
		elseif (t.m.visuals[effect_mode] or {})[effect_type] then
			error(t.m.name .. ' is only allowed one %s visual effect.':format(key))
		elseif opposing_modes[effect_mode] and (t.m.visuals[opposing_modes[effect_mode][1]] or t.m.visuals[opposing_modes[effect_mode][2]]) then 
			error(t.m.name .. ' has an effect that opposes %s. (modes: %s)':format(key,T(t.m.visuals):keyset():concat(', ')))
		elseif data[2] == nil and effect_type ~= 'arrow' then
			error('"%s" visual effects require an associated value.':format(key))
		else
			t.m.visuals[effect_mode] = t.m.visuals[effect_mode] or {}
			t.m.visuals[effect_mode][effect_type] = data[2] or true
			if effect_type == 'click_toggle' then
				t.m.toggle_state = (function() if removekey(data[2], 'default') == true then return true else return false end end)()
				t.m.toggle_colors = data[2].colors
				fx.click_toggle(t.m, t.m.toggle_state)
			end
		end
	end
end

function groups.unregister_visual(t, key) --untested
	if t.m.class ~= 'images' and tm.class ~= 'texts' then
		return error('An image or text must be the first argument of register_visual.')
	end
	local key_parts = key:split('_')
	local effect_mode, effect_type = key_parts:remove(1), key_parts:concat('_')
	if not visual_keys[key] then
		error('Visual effect "%s" is not available for group elements.':format(key))
	elseif not (t.m.visuals[effect_mode] or {})[effect_type] then
		error(t.m.name .. ' has no %s visual effect.':format(key))
	end
	t.m.visuals[effect_mode][effect_type] = nil
	if T(t.m.visuals[effect_mode]):empty() then
		t.m.visuals[effect_mode] = nil
	end
end

function groups.register_event(t, ...)
	if t.m.class ~= 'images' and t.m.class ~= 'texts' then
		return error('An image or text must be the first argument of register_event.')
	end
	for _, data in ipairs({...}) do
		local key = event_keys_map[data[1]] or data[1]
		local key_parts = key:split('_')
		local event_mode, event_type = key_parts:remove(1), key_parts:concat('_')
		if not event_keys[key] then
			error('Event %s not available for group elements.':format(key))
		elseif opposing_modes[event_mode] and (t.m.events[opposing_modes[event_mode][1]] or t.m.events[opposing_modes[event_mode][2]]) then 
			error('"%s" has an event mode that opposes %s. (event modes: %s)':format(t.m.name,key,T(t.m.events):keyset():concat(', ')))
		elseif data[2] == nil then
			error('All events require an associated function.':format(key))
		else
			t.m.events[event_mode] = t.m.events[event_mode] or {}
			t.m.events[event_mode][event_type] = t.m.events[event_mode][event_type] or {}
			t.m.events[event_mode][event_type][#t.m.events[event_mode][event_type] + 1] = data[2]
			return #t.m.events[event_mode][event_type]
		end
	end
end

function groups.unregister_event(m, key, func)
	local key = event_keys_map[key] or key
	local key_parts = key:split('_')
	local event_mode, event_type = key_parts:remove(1), key_parts:concat('_')
	if not (event_keys[key] and (m.events[event_mode] or {}).event_type) then
		return error('"%s" event does not exist on the "%s" group element.':format(key, m.name))
	end
	
	if type(func) == 'number' then
		table.remove(m.events[event_mode][event_type], func)
	else
		for i, fn in ipairs(meta[t].events[event_mode][event_type]) do
			if fn == func then
				table.remove(meta[t].events[key], i)
				return
			end
		end
	end
end

local function auto_show_hide(show_or_hide)
	for w, wm in pairs(cache.groups) do
		wm.auto_hidden = not show_or_hide
		if wm.auto_hide then
			for _, t in ipairs(wm.elements) do
				if wm.visible then
					w:visible(show_or_hide, true) --pass in optional var to avoid messing with visible flags
				end
			end
		end
	end
end

windower.register_event('incoming chunk', function(id, data, modified, injected, blocked)
	if id == 0x00B then -- zone/log out
		auto_show_hide(false)
	elseif id == 0x00A then --zone/log in
		auto_show_hide(true)
	end
end)



return groups