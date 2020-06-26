import "./main.css";
import * as xmlrpc from "xmlrpc";
import * as Odoo from "odoo-xmlrpc";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

var app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: [
    localStorage.getItem("orders") || "",
    localStorage.getItem("odooPassword") || ""
  ]
});

app.ports.storeOrders.subscribe(function (orders) {
  localStorage.setItem("orders", JSON.stringify(orders));
});

app.ports.storeCustomers.subscribe(function (customers) {
  localStorage.setItem("customers", JSON.stringify(customers));
});

app.ports.storePassword.subscribe(function (password) {
  localStorage.setItem("odooPassword", password);
});

app.ports.retrieveStockFromServer.subscribe(function (useless) {
  getStockFromOdoo();
});

app.ports.retrieveCustomersFromServer.subscribe(function (useless) {
  getCustomers();
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();


var odoo = new Odoo({
    url: "https://odooproxy.vieuxsinge.com/xmlrpc/2",
    db: "brasserieduvieuxsinge",
    username: "contact@vieuxsinge.com",
    password: localStorage.getItem("odooPassword")
});

function getStockFromOdoo() {
  odoo.connect(function (err) {
    if (err) { return console.log(err); }

    odoo.execute_kw(
      'product.template',
      'search_read',
      [
        [[['x_volume', '>=', '0.1']]],
        {'fields': ['x_beername', 'qty_available', 'x_volume']}
      ], function(err, items) {
        app.ports.updateStock.send(JSON.stringify(items));
      });
    });
}

function getCustomers() {
  odoo.connect(function (err) {
    if (err) { return console.log(err); }

    odoo.execute_kw(
      'res.partner',
      'search_read',
      [
        [[['is_company', '=', true],['customer', '=', true]]],
        {'fields': ['name']}
      ], function(err, items) {
        app.ports.updateCustomers.send(JSON.stringify(items));
      });
    });
}
