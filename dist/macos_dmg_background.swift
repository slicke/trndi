#!/usr/bin/env swift
import Foundation
import CoreGraphics
import ImageIO

let width = 600
let height = 400

let iconCenterY: CGFloat = 200
let trndiX: CGFloat = 150
let appsX: CGFloat = 450
let iconHalf: CGFloat = 64

let arrowStartX = trndiX + iconHalf + 12
let arrowEndX = appsX - iconHalf - 12
let arrowY = CGFloat(height) - iconCenterY

let outputPath = CommandLine.arguments.count > 1
    ? CommandLine.arguments[1]
    : "background.png"

guard let colorSpace = CGColorSpace(name: CGColorSpace.sRGB),
      let ctx = CGContext(
        data: nil,
        width: width,
        height: height,
        bitsPerComponent: 8,
        bytesPerRow: 0,
        space: colorSpace,
        bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue
      ) else {
    fputs("Failed to create CGContext\n", stderr)
    exit(1)
}

ctx.setFillColor(red: 0.97, green: 0.97, blue: 0.98, alpha: 1.0)
ctx.fill(CGRect(x: 0, y: 0, width: width, height: height))

let arrowR: CGFloat = 0.35
let arrowG: CGFloat = 0.35
let arrowB: CGFloat = 0.40
let arrowA: CGFloat = 0.85

ctx.setStrokeColor(red: arrowR, green: arrowG, blue: arrowB, alpha: arrowA)
ctx.setFillColor(red: arrowR, green: arrowG, blue: arrowB, alpha: arrowA)
ctx.setLineWidth(6)
ctx.setLineCap(.round)

ctx.move(to: CGPoint(x: arrowStartX, y: arrowY))
ctx.addLine(to: CGPoint(x: arrowEndX - 12, y: arrowY))
ctx.strokePath()

let head: CGFloat = 24
ctx.move(to: CGPoint(x: arrowEndX, y: arrowY))
ctx.addLine(to: CGPoint(x: arrowEndX - head, y: arrowY + head * 0.6))
ctx.addLine(to: CGPoint(x: arrowEndX - head, y: arrowY - head * 0.6))
ctx.closePath()
ctx.fillPath()

guard let cgImage = ctx.makeImage() else {
    fputs("Failed to make image\n", stderr)
    exit(1)
}

let url = URL(fileURLWithPath: outputPath) as CFURL
let pngType = "public.png" as CFString
guard let destination = CGImageDestinationCreateWithURL(url, pngType, 1, nil) else {
    fputs("Failed to create CGImageDestination at \(outputPath)\n", stderr)
    exit(1)
}
CGImageDestinationAddImage(destination, cgImage, nil)
if !CGImageDestinationFinalize(destination) {
    fputs("Failed to finalize PNG at \(outputPath)\n", stderr)
    exit(1)
}
