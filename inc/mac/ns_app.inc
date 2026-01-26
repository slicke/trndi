{ Custom NSApplicationDelegate class }
TMyAppDelegate = objcclass(NSObject, NSApplicationDelegateProtocol)
public
  function applicationShouldHandleReopen_hasVisibleWindows(
    sender: NSApplication; hasVisibleWindows: Boolean): Boolean; message 'applicationShouldHandleReopen:hasVisibleWindows:';
  function applicationDockMenu(sender: NSApplication): NSMenu; message 'applicationDockMenu:';
  procedure miSettingsMacClick(sender: id); message 'miSettings:';

  // AppleScript / Apple Event handlers (DARWIN only)
  procedure handleGetCurrentUser_withReplyEvent_(command: id; replyEvent: id); message 'handleGetCurrentUser:withReplyEvent:';
  procedure handleGetCurrentNickname_withReplyEvent_(command: id; replyEvent: id); message 'handleGetCurrentNickname:withReplyEvent:';
  procedure handleGetCurrentReading_withReplyEvent_(command: id; replyEvent: id); message 'handleGetCurrentReading:withReplyEvent:';
  procedure handlePredictReadings_withReplyEvent_(command: id; replyEvent: id); message 'handlePredictReadings:withReplyEvent:';
end;
{** Darwin-specific declarations above }
