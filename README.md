# Groups-Library
A custom images and texts grouping library meant to make grouping and managing clusters of images and texts easier for addon authors.

### Demo
Please visit my [UI Mockup (Groups Lib Demos)](https://github.com/Roland-J/UI-Mockup-Groups-Lib-Demos-) repository for an interactive demo! It is an addon written with 5 sample UI scenarios, each showcasing many different aspects of this library, its capabilities, and how to write the required code.


#### Wiki Section
0. Groups & Their Elements: This library allows you to create groups of elements, specifically groups of
     images and texts objects. A group is comprised of its elements and its anchor. Its anchor is dict
	 tated by its alignment. (see next point)
1. Alignment: This library supports vertical - top/center/bottom - and horizontal - left/center/right -
     alignment. Traditional elements like texts have a top-left ANCHOR, meaning that a pos 0/0 text has
	 and anchor of 0/0. With alignment, however, the anchor moves. So, a top.right aligned group's
	 anchor will be the pixel in the top-right corner of your screen. A center/center aligned group's
	 anchor will be the center of your screen. This allows your groups to find these dynamic anchor
	 locations effortlessly, without a lot of coding required on your part. To allow further positioning,
	 the alignment mechanism also supports x/y OFFSETS. A top/right aligned group with a -40/-20 offset
	 will appear with its anchor 40 pixels from the right of your screen and 20 pixels from the top of
	 your screen.
2. UI Scalar: This library supports UI scalars. Both the previously mentioned OFFSETS and your defined
     element sizes will be scaled with your current UI scalar. This means that for users with large 4k
	 monitors and a large x2.5 UI scalar, this lib's groups will grow to be 2.5x larger than normal, so
	 that they do not render at a miniscule size compared to your UI scalar. This also allows offsets to
	 keep pace with the vanilla UI's offsets, since the vanilla UI's offsets get scaled too BTW. This is
	 why this demo's vanilla_ui mockup's top-right anchor stays synced with the actual vanilla UI's top-
	 right corner, as both are scaling with your UI scalar!
3. Visuals: This library supports many default visuals! This means that you can easily add visuals to
     your elements. The code below demonstrates how to do so. The provided visuals are:
	     left_click_travel,   left_click_toggle,   left_click_bulge,   left_click_tint,
	     right_click_travel,  right_click_toggle,  right_click_bulge,  right_click_tint,
	     middle_click_travel, middle_click_toggle, middle_click_bulge, middle_click_tint,
	     scroll_zoom, hover_travel, hover_bulge, hover_tint, hover_arrow, hover_color
4. Zoom: As you may have noticed in the visuals, this library supports zooming in/out of a group via the
     mouse scroll wheel. The zoom feature provides three "focus" settings:
         1. No focus: The default focus, given when no specific focus is declared. In this mode, the
                 given group will grow outward from its anchor. A top/left aligned group will grow down
				 wardand rightward, a center/center aligned group will grow in all directions, whilst a
				 bottom/right aligned group will grow upward/leftward.
		 2. Center focus: Regardless of the group's alignment, it will grow outward from its center. I
		         am expecting this to be the most popular zoom focus.
		 3. Cursor focus: Regardless of the group's alignment, it will grow outward from the cursor. This
		         would be useful for map UIs, for certain.
5. Events: Along with visuals, this library supports adding events to any element. See the below sample
     code for the syntax to use. This allows you to run your own function when an element is clicked,
	 released, hovered, or scrolled. With events, images and texts can easily become interactable!
6. Drag Sync: All the elements within a group are dragged together when any one of its elements is drag
     ged! This allows you to create clusters of elements easily allow them to be both clickable and drag
	 gable, without having to fuss with any complex drag logic!
7. Drag Bounds: This feature allows you to define optional bounds which groups cannot be dragged outside
     of! This will hopefully provide an intuitive and user-friendly way of helping keep the groups on
	 screen, assuming that users wouldn't want to drag a group offscreen.
8. group Sync: This feature allows you to sync multiple groups together. When synced, all groups will
     A) zoom in/out together, B) drag together, and C) calculate their drag bounds together. This is use
	 ful if you have multiple groups where one main element's buttons show/hide the other groups, similar
	 to this demo's vanilla_ui demo UI.
9. Config/Saving: This library automatically saves group positions and zoom levels. Any addon utilizing
     this library will therefore inherently support persistent user preferences regarding group position
	 and zoom! Hurray!
10. Auto-Hide: This library also supports auto-hide. This means that while zoning - both between zones and
     during login/logout - any groups that have auto-hide enabled will disappear when zoning out and re
	 appear when zoning in.
BTW: Feel free to re-use my Button002-NeutralLight2.png asset.
