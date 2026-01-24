// Test setTimeout/setInterval with named functions

function testCallback() {
    Trndi.alert("Timer fired successfully!");
}

// Test setTimeout
Trndi.alert("Starting setTimeout test...");
var timerId = setTimeout(testCallback, 3000);
Trndi.alert("setTimeout created with ID: " + timerId);
