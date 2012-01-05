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
import java.util.ArrayList;

import org.mozilla.javascript.Context;
import org.mozilla.javascript.ContextFactory;
import org.mozilla.javascript.ImporterTopLevel;
import org.mozilla.javascript.JavaScriptException;
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
			try {
				handlePOST(exchange);
				respondOkTo(exchange);
			} catch (Exception e) {
				e.printStackTrace();
				respondErrorTo(exchange);
			}
		}
	}

	private void handlePOST(HttpExchange exchange) throws IOException {
		InputStream is = exchange.getRequestBody();
		byte[] data = new byte[is.available()];
		is.read(data);
		is.close();
		String stringdata = new String(data);

		String uri = exchange.getRequestURI().toASCIIString().replaceFirst("/", "");

		if (uri.equals("reload")) {

		} else if (uri.startsWith("createProgram")) {
			String[] params = uri.split("\\?");
			String main = params[1];
			String targetFilename = params[2];

			ArrayList<String> files = new ArrayList<String>();

			String[] kernelFiles = new String[] { "boot", "Kernel-Objects", "Kernel-Classes", "Kernel-Methods",
					"Kernel-Collections", "Kernel-Exceptions", "Kernel-Transcript", "parser", "Compiler" };

			for (String filename : kernelFiles) {
				files.add(filename);
			}

			for (String entry : stringdata.split("&")) {
				files.add(entry.split("=")[1]);
			}

			files.add("init");

			FileOutputStream fos = new FileOutputStream(targetFilename + ".js");

			for (String filename : files) {
				writeProgram(fos, filename);
			}

			fos.write(("smalltalk." + main + "._main()").getBytes());
			fos.close();
		}
	}

	private void writeProgram(FileOutputStream fos, String filename) throws FileNotFoundException, IOException {
		byte[] data;
		FileInputStream fis = new FileInputStream("js/" + filename + ".js");
		data = new byte[fis.available()];
		fis.read(data);
		fis.close();

		fos.write(data);
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
		exchange.close();
	}

	private void writeResponse(HttpExchange exchange, int httpStatus, String contentType, byte response[])
			throws IOException {
		exchange.sendResponseHeaders(httpStatus, response.length);
		OutputStream os = exchange.getResponseBody();
		os.write(response);
		os.flush();
		os.close();
		exchange.getResponseHeaders().add("Content-type", contentType);
		exchange.close();
	}

	public static void main(String[] args) {
		if(args.length == 0) {
			startServer();
		} else {
			try {
				runScript(args[0]);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

	}

	public static void runScript(String filename) throws IOException {
		ContextFactory.initGlobal(new AmberContextFactory());
		Context ctx = Context.enter();
		ctx.setOptimizationLevel(-1);

		String script = readFile(filename);

		ScriptableObject scope = new ImporterTopLevel(ctx);
		
//		ScriptableObject scope = ctx.initStandardObjects();

		Object o = ctx.evaluateString(scope, script, "<cmd>", 1, null);
		System.out.println(o);

		// ScriptableObject.putProperty(scope, "logger", logger);
	}

	public static String readFile(String filename) throws FileNotFoundException, IOException {
		File f = new File(filename);
		FileInputStream fis = new FileInputStream(f);
		byte[] content = new byte[fis.available()];
		fis.read(content);
		String jsContent = new String(content, "UTF-8");
		return jsContent;
	}

	public static void startServer() {
		System.out.println("Trying to start an Amber-Server ...");
		try {
			 System.setProperty("java.net.preferIPv6Addresses", "false");
			 _server = HttpServer.create(new InetSocketAddress(8080), 0);
			 _server.createContext("/", new FileServer());
			 _server.setExecutor(null); // creates a default executor
			 _server.start();
			 System.out.println("Server started, listening on Port 8080");
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public static void stopServer() {
		_server.stop(0);
	}

	public static class AmberContextFactory extends ContextFactory {
		@Override
		protected boolean hasFeature(Context cx, int featureIndex) {
			switch (featureIndex) {
			case Context.FEATURE_RESERVED_KEYWORD_AS_IDENTIFIER:
				return true;
			}

			return super.hasFeature(cx, featureIndex);
		}
	}
	
	
	public static class JavaFactory {
		public Object createInstance(String className) {
			Object obj = null;
			try {
				obj = Class.forName(className).newInstance();
			} catch (Exception e) {
				e.printStackTrace();
			}
			return obj;
		}
	}
}
