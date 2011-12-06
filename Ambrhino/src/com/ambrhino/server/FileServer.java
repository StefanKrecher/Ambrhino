package com.ambrhino.server;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.URLConnection;

import org.mozilla.javascript.Context;
import org.mozilla.javascript.ScriptableObject;



import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

public class FileServer implements HttpHandler {

	private static HttpServer _server;

	@Override
	public void handle(HttpExchange exchange) throws IOException {

		if (exchange.getRequestMethod().equals("GET")) {
			handleGET(exchange);
		} else if (exchange.getRequestMethod().equals("PUT")) {
			handlePUT(exchange);
		} else if (exchange.getRequestMethod().equals("POST")) {
			handlePOST(exchange);
		}
	}

	private void handlePOST(HttpExchange exchange) throws IOException {
		InputStream is = exchange.getRequestBody();
		byte[] data = new byte[is.available()];
		is.read(data);
		is.close();
		String stringdata = new String(data);
		if(stringdata.equals("reload")) {
			
		} 
	}

	private void handlePUT(HttpExchange exchange) throws IOException {
		try {
			String fileName = exchange.getRequestURI().toASCIIString().replaceFirst("/", "");
			
			InputStream is = exchange.getRequestBody();
			byte[] data = new byte[is.available()];
			is.read(data);
			is.close();
			String stringdata = new String(data);
			
			FileOutputStream fos = new FileOutputStream(fileName);
			fos.write(stringdata.getBytes());
			fos.flush();
			fos.close();
			respondOkTo(exchange);
		} catch (Exception e) {
			respondErrorTo(exchange);
		}
	}

	private void handleGET(HttpExchange exchange) throws IOException {
		String fileName = exchange.getRequestURI().toASCIIString();
		fileName = fileName.replaceFirst("/", "").split("\\?")[0];
		File f = new File(fileName);
		if (f.exists()) {
			respondFileTo(exchange, f);
		} else {
			respondNotFoundTo(exchange);
		}
	}

	private void respondFileTo(HttpExchange exchange, File f) throws IOException {
		FileInputStream fis = new FileInputStream(f);
		byte[] data = new byte[fis.available()];
		fis.read(data);
		fis.close();

		String fileName = f.getName();
		String contentType = URLConnection.guessContentTypeFromName(fileName);

		if (contentType == null) {
			if (fileName.endsWith("css")) {
				contentType = "text/css";
			}
			if (fileName.endsWith("js")) {
				contentType = "text/javascript";
			}
		}
		System.out.println(fileName + " : " + contentType);
		writeResponse(exchange, 200, contentType, data);
	}

	private void respondNotFoundTo(HttpExchange exchange) throws IOException {
		String response = "404 - not found";
		writeResponse(exchange, 404, "text/plain", response.getBytes());
	}
	private void respondErrorTo(HttpExchange exchange) throws IOException {
		String response = "500 - internal server error";
		writeResponse(exchange, 500, "text/plain", response.getBytes());
	}
	private void respondOkTo(HttpExchange exchange) throws IOException {
		exchange.getResponseHeaders().add("Content-type", "text/plain");
		exchange.sendResponseHeaders(200, 0);
	}

	private void writeResponse(HttpExchange exchange, int httpStatus, String contentType, byte response[])
			throws IOException {
		exchange.sendResponseHeaders(httpStatus, response.length);
		OutputStream os = exchange.getResponseBody();
		os.write(response);
		os.flush();
		os.close();
		exchange.getResponseHeaders().add("Content-type", contentType);
	}

	public static void main(String[] args) {
		start();
	}

	
	public static void initScript() throws IOException {
		Context ctx = Context.enter();
		ctx.setOptimizationLevel(-1);
		
		String script = readFile("amber.js");
		
		ScriptableObject scope = ctx.initStandardObjects();
		ctx.evaluateString(scope, "var CanvasRenderingContext2D = false;", "<cmd>", 1, null);
		ctx.evaluateString(scope, "var isServer = true;", "<cmd>", 1, null);
		ctx.evaluateString(scope, "var javaobjects = new java.util.HashMap();", "<cmd>", 1, null);
		ctx.evaluateString(scope, script, "<cmd>", 1, null);
//		String scriptFromFile = readFile("js/init.js");
//		ctx.evaluateString(scope, scriptFromFile, "<cmd>", 1, null);

//		ScriptableObject.putProperty(scope, "logger", logger);
	}
	
	public static String readFile(String filename) throws FileNotFoundException, IOException {
		File f = new File(filename);
		FileInputStream fis = new FileInputStream(f);
		byte[] content = new byte[fis.available()];
		fis.read(content);
		String jsContent = new String(content, "UTF-8");
		return jsContent;
	}
	
	public static void start() {
		try {
			System.setProperty("java.net.preferIPv6Addresses", "false");
			_server = HttpServer.create(new InetSocketAddress(8080), 0);
			_server.createContext("/", new FileServer());
			_server.setExecutor(null); // creates a default executor
			_server.start();
			
			initScript();
			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public static void stop() {
		_server.stop(0);
	}
}
