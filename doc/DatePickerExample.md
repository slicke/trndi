# ExtDatePicker Usage Example

The `ExtDatePicker` function provides a date picker dialog with optional min/max date constraints.

## Function Signature

```pascal
function ExtDatePicker(
  const dialogsize: TUXDialogSize;
  const ACaption, ATitle, ADesc: string;
  ADefault: TDateTime;
  AMinDate: TDateTime;
  AMaxDate: TDateTime;
  var ModalResult: TModalResult;
  const icon: UXImage = uxmtCog
): TDateTime;
```

## Parameters

- **dialogsize**: Layout preset (`uxdNormal`, `uxdBig`, `uxdAuto`, etc.)
- **ACaption**: Window caption
- **ATitle**: Title text displayed in the dialog
- **ADesc**: Description text
- **ADefault**: Initial date value to display
- **AMinDate**: Minimum allowed date (pass 0 to disable)
- **AMaxDate**: Maximum allowed date (pass 0 to disable)
- **ModalResult**: Output parameter with modal result (mrOk, mrCancel)
- **icon**: Optional emoji icon (defaults to gear/cog)

## Return Value

Returns the selected date if user clicks OK, otherwise returns the default date.

## Example Usage

### Basic Date Picker (No Constraints)

```pascal
var
  selectedDate: TDateTime;
  modalRes: TModalResult;
begin
  selectedDate := ExtDatePicker(
    uxdNormal,
    'Select Date',
    'Choose a date',
    'Please select a date from the calendar',
    Now,           // Default to today
    0,             // No minimum date
    0,             // No maximum date
    modalRes
  );
  
  if modalRes = mrOk then
    ShowMessage('You selected: ' + DateToStr(selectedDate));
end;
```

### Date Picker with Min/Max Constraints

```pascal
var
  selectedDate: TDateTime;
  modalRes: TModalResult;
  minDate, maxDate: TDateTime;
begin
  // Allow selection only within the next 30 days
  minDate := Now;
  maxDate := Now + 30;
  
  selectedDate := ExtDatePicker(
    uxdNormal,
    'Schedule Appointment',
    'Select Appointment Date',
    'Choose a date within the next 30 days',
    Now,
    minDate,
    maxDate,
    modalRes,
    uxmtInformation
  );
  
  if modalRes = mrOk then
    ShowMessage('Appointment scheduled for: ' + DateToStr(selectedDate));
end;
```

### Date Picker for Past Events

```pascal
var
  selectedDate: TDateTime;
  modalRes: TModalResult;
begin
  // Only allow dates in the past
  selectedDate := ExtDatePicker(
    uxdBig,                    // Use big layout for touch screens
    'Birth Date',
    'Enter Your Birth Date',
    'Select your date of birth',
    EncodeDate(1990, 1, 1),   // Default date
    EncodeDate(1900, 1, 1),   // Min: 1900
    Now,                       // Max: today
    modalRes
  );
  
  if modalRes = mrOk then
    ShowMessage('Birth date: ' + DateToStr(selectedDate));
end;
```

## Features

- **Dark Mode Support**: Automatically adapts to Windows dark mode
- **Touch-Friendly**: Use `uxdBig` or `uxdAuto` for touch screen layouts
- **Min/Max Date Constraints**: Optional minimum and maximum date limits
- **Keyboard Navigation**: Supports Enter to confirm, Escape to cancel
- **Cross-Platform**: Works on Windows, Linux, and other platforms supported by Lazarus

## Notes

- Pass `0` for `AMinDate` or `AMaxDate` to disable those constraints
- The function uses the `TDateEdit` component from Lazarus (`EditBtn` unit)
- Date format follows the system locale settings
- The dialog size can be adjusted using different `TUXDialogSize` values
