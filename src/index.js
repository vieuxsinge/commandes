import "./main.css";
import * as xmlrpc from "xmlrpc";
import * as Odoo from "odoo-xmlrpc";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

var app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: {
      encodedOrders : localStorage.getItem("orders") || ""
    , encodedPassword : localStorage.getItem("odooPassword") || ""
    , encodedCustomers : localStorage.getItem("customers") || ""
    , encodedStock : localStorage.getItem("stock") || ""
  }
});

app.ports.storeOrders.subscribe(function (orders) {
  localStorage.setItem("orders", JSON.stringify(orders));
});

app.ports.storeCustomers.subscribe(function (customers) {
  localStorage.setItem("customers", JSON.stringify(customers));
});

app.ports.storeStock.subscribe(function (stock) {
  localStorage.setItem("stock", JSON.stringify(stock));
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

app.ports.createOrdersOnServer.subscribe(function(orders) {
  orders.forEach(createSaleOrder);
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
      'product.product',
      'search_read',
      [
        [[['x_volume', '>=', '0.1']]],
        {'fields': ['x_beername', 'qty_available', 'x_volume', 'name', 'default_code']}
      ], function(err, items) {
        console.log(items);
        app.ports.gotStockFromServer.send(JSON.stringify(items));
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
        app.ports.gotCustomersFromServer.send(JSON.stringify(items));
      });
    });
}

function createOrder(order) {
  console.log("create order", order);

}

function createSaleOrder(order){

  odoo.connect(function (err) {
    if (err) { return console.log(err); }

    odoo.execute_kw(
      'sale.order',
      'create',
      [[{ partner_id: order.customer.id }]],
      function (err, serverOrderId) {
        if (err) {
          return console.log(err);
        }
        order.orders.forEach((line) => {
          createSaleOrderLine(serverOrderId, line);
        });
      });
    });
}

function createSaleOrderLine(orderId, line) {
  odoo.execute_kw(
    'sale.order.line',
    'create',
    [[{
      order_id: orderId,
      product_id: line.beer.id,
      product_uom_qty: line.quantity
    }]],
    function (err, value) {
      if (err) {
        return console.log(err);
      }
      console.log('Result: ', value);
    },
  );
}
