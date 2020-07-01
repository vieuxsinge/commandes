import "./main.css";
import * as xmlrpc from "xmlrpc";
import * as Odoo from "odoo-xmlrpc";
import AwaitLock from 'await-lock';
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

const crypto = window.crypto || window.msCrypto;

const getRandomInts = (n) => {
    const randInts = new Uint32Array(n);
    crypto.getRandomValues(randInts);
    return Array.from(randInts);
};
const randInts = getRandomInts(5);

var app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: {
      encodedOrders : localStorage.getItem("orders") || ""
    , encodedPassword : localStorage.getItem("odooPassword") || ""
    , encodedCustomers : localStorage.getItem("customers") || ""
    , encodedStock : localStorage.getItem("stock") || ""
    , seed: randInts[0]
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

app.ports.createOrdersOnServer.subscribe(async function(orders) {
  orders.forEach(await createSaleOrder);
});

serviceWorker.unregister();

var odoo = new Odoo({
    url: "https://odooproxy.vieuxsinge.com/xmlrpc/2",
    db: "brasserieduvieuxsinge",
    username: "contact@vieuxsinge.com",
    password: localStorage.getItem("odooPassword")
});

var lock = new AwaitLock();

const callOdoo = async (model, method, params) => {
  return new Promise(async (resolve, reject) => {
    odoo.connect(async (err) => {
      if (err) reject(new Error(err));
      await lock.acquireAsync();
      odoo.execute_kw(model, method, params, (err, value) => {
        lock.release();
        if (err) {
          reject(new Error(err));
        }
        console.log(value);
        resolve(value);
      });
    });
  });
}

async function getStockFromOdoo() {
  let items = await callOdoo(
    'product.product',
    'search_read',
    [
      [[['x_volume', '>=', '0.1']]],
      {'fields': ['x_beername', 'qty_available', 'x_volume', 'name', 'default_code']}
    ]
  );
  app.ports.gotStockFromServer.send(JSON.stringify(items));
}

async function getCustomers() {
  let items = await callOdoo(
    'res.partner',
    'search_read',
    [
      [[['is_company', '=', true],['customer', '=', true]]],
      {'fields': ['name']}
    ]);
  app.ports.gotCustomersFromServer.send(JSON.stringify(items));
}

async function createSaleOrder(order){
  let orderId = await callOdoo(
    'sale.order',
    'create',
    [[{ partner_id: order.customer.id }]]
  );
  order.orders.forEach(async (line) => {
    await callOdoo(
      'sale.order.line',
      'create',
      [[{'order_id': orderId, 'product_id': line.beer.id, 'product_uom_qty': line.quantity}]]);
  });
  app.ports.gotOrderIdFromServer.send(JSON.stringify(
    {
      'localId' : order.localId
      , 'remoteId' : orderId
  }));
}
