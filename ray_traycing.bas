CONST MATH_INF# = 10000000

CONST COLOR_WHITE~& = _RGB32(32, 32, 32)


TYPE CanvasConfigType
    numColors AS INTEGER
    width AS INTEGER
    height AS INTEGER
    halfWidth AS INTEGER
    halfHeight AS INTEGER
END TYPE


TYPE CanvasPointType
    'A 2d point
    x AS INTEGER
    y AS INTEGER
END TYPE


TYPE ViewportPointType
    'A 3d point
    x AS DOUBLE
    y AS DOUBLE
    z AS DOUBLE
END TYPE


TYPE ViewportConfigType
    width AS DOUBLE
    height AS DOUBLE
    distance AS DOUBLE
    origin AS ViewportPointType
END TYPE


TYPE SphereType
    center AS ViewportPointType
    radius AS DOUBLE
    color AS _UNSIGNED LONG
    specular AS DOUBLE
    reflective AS DOUBLE
END TYPE


TYPE SphereIntersectionType
    t1 AS DOUBLE
    t2 AS DOUBLE
END TYPE


TYPE LightType
    source AS STRING 'Ambient, Directional, Point
    intensity AS DOUBLE
    position AS ViewportPointType
    direction AS ViewportPointType
END TYPE


TYPE ClosestIntersectionReturnType
    closestSphere AS SphereType
    closestT AS DOUBLE
END TYPE


DIM center AS ViewportPointType
DIM spheres(1 TO 4) AS SphereType
'Sphere 1
center.x = 0
center.y = -1
center.z = 3
DIM sphere1 AS SphereType
sphere1.center = center
sphere1.radius = 1
sphere1.color = _RGB32(&H8B, &H5C, &HF6)
sphere1.specular = 50
sphere1.reflective = 0.2
spheres(1) = sphere1

'Sphere 2
center.x = 2
center.y = 0
center.z = 4
DIM sphere2 AS SphereType
sphere2.center = center
sphere2.radius = 1
sphere2.color = _RGB32(&HEC, &H48, &H99)
sphere2.specular = 500
sphere2.reflective = 0.3
spheres(2) = sphere2

'Sphere 3
center.x = -2
center.y = 0
center.z = 4
DIM sphere3 AS SphereType
sphere3.center = center
sphere3.radius = 1
sphere3.color = _RGB32(&H10, &HB9, &H81)
sphere3.specular = 5
sphere3.reflective = 0.4
spheres(3) = sphere3

'Sphere 4
center.x = 0
center.y = -5001
center.z = 0
DIM sphere4 AS SphereType
sphere4.center = center
sphere4.radius = 5000
sphere4.color = _RGB32(&H60, &HA5, &HFA)
sphere4.specular = 1000
sphere4.reflective = 0.5
spheres(4) = sphere4


DIM lights(1 TO 3) AS LightType

DIM ambientLight AS LightType
ambientLight.source = "Ambient"
ambientLight.intensity = 0.15
lights(1) = ambientLight

DIM pointLight AS LightType
pointLight.source = "Point"
pointLight.intensity = 0.5
DIM lightPosition AS ViewportPointType
lightPosition.x = 2
lightPosition.y = 1
lightPosition.z = 0
pointLight.position = lightPosition
lights(2) = pointLight

DIM directionalLight AS LightType
directionalLight.source = "Directional"
directionalLight.intensity = 0.2
DIM lightDirection AS ViewportPointType
lightDirection.x = 1 '-3
lightDirection.y = 4 '1
lightDirection.z = 4 '-3
directionalLight.direction = lightDirection
lights(3) = directionalLight


DIM canvasConfig AS CanvasConfigType
canvasConfig.numColors = 32
canvasConfig.width = 800
canvasConfig.height = 600
canvasConfig.halfWidth = canvasConfig.width \ 2
canvasConfig.halfHeight = canvasConfig.height \ 2

DIM cameraOrigin AS ViewportPointType
cameraOrigin.x = 0
cameraOrigin.y = 0
cameraOrigin.z = 0
DIM viewportConfig AS ViewportConfigType
viewportConfig.width = 1.4
viewportConfig.height = 1
viewportConfig.distance = 0.75
viewportConfig.origin = cameraOrigin


CALL Main(spheres(), lights(), canvasConfig, viewportConfig)

SUB Main (spheres() AS SphereType, lights() AS LightType, canvasConfig AS CanvasConfigType, viewportConfig AS ViewportConfigType)
    testResults = RunTests
    IF testResults <> 0 THEN
        PRINT "Main - red tests, aborting."
        PRINT
        PRINT "Press any key to exit."
        DO
        LOOP UNTIL INKEY$ <> ""
        SYSTEM
    END IF

    CALL InitCanvas(canvasConfig)

    DIM pressedKey AS STRING
    DIM change AS INTEGER
    change = 1
    DIM dimension AS STRING
    dimension = "x"
    DO
        IF change <> 0 THEN
            CLS
            CALL Render(spheres(), lights(), canvasConfig, viewportConfig)
            change = 0
        END IF

        pressedKey = ""
        pressedKey = INKEY$

        IF RIGHT$(pressedKey, 1) = "K" THEN 'Left arrow pressed
            IF dimension = "x" THEN
                viewportConfig.origin.x = viewportConfig.origin.x + 1
            ELSEIF dimension = "y" THEN
                viewportConfig.origin.y = viewportConfig.origin.y + 1
            ELSEIF dimension = "z" THEN
                viewportConfig.origin.z = viewportConfig.origin.z + 1
            END IF
            change = change + 1
        ELSEIF RIGHT$(pressedKey, 1) = "M" THEN 'Right arrow pressed
            IF dimension = "x" THEN
                viewportConfig.origin.x = viewportConfig.origin.x - 1
            ELSEIF dimension = "y" THEN
                viewportConfig.origin.y = viewportConfig.origin.y - 1
            ELSEIF dimension = "z" THEN
                viewportConfig.origin.z = viewportConfig.origin.z - 1
            END IF
            change = change + 1
        ELSEIF RIGHT$(pressedKey, 1) = "H" THEN 'Up arrow pressed
            viewportConfig.distance = viewportConfig.distance + 0.125
            change = change + 1
        ELSEIF RIGHT$(pressedKey, 1) = "P" THEN 'Down arrow pressed
            viewportConfig.distance = viewportConfig.distance - 0.125
            change = change + 1
        ELSEIF pressedKey = "x" THEN
            dimension = "x"
        ELSEIF pressedKey = "y" THEN
            dimension = "y"
        ELSEIF pressedKey = "z" THEN
            dimension = "z"
        END IF
    LOOP UNTIL pressedKey = CHR$(27) OR pressedKey = "q" 'ESC
    SYSTEM
END SUB

FUNCTION RunTests ()
    result = TestAdjustLightIntensity
    IF result <> 0 THEN
        RunTests = result
        EXIT FUNCTION
    END IF

    RunTests = 0
END FUNCTION

SUB InitCanvas (config AS CanvasConfigType)
    CLS
    SCREEN _NEWIMAGE(config.width, config.height, config.numColors)
END SUB

SUB Render (spheres() AS SphereType, lights() AS LightType, canvasConfig AS CanvasConfigType, viewportConfig AS ViewportConfigType)
    DIM cameraOrigin AS ViewportPointType
    cameraOrigin = viewportConfig.origin

    DIM position AS CanvasPointType
    DIM rayDirection AS ViewportPointType
    DIM pixelColor AS _UNSIGNED LONG
    FOR x = -canvasConfig.halfWidth TO canvasConfig.halfWidth
        position.x = x
        FOR y = -canvasConfig.halfHeight TO canvasConfig.halfHeight
            position.y = y
            CALL CanvasToViewPort(position, rayDirection, canvasConfig, viewportConfig)
            pixelColor = TraceRay~&(spheres(), lights(), cameraOrigin, rayDirection, 1, MATH_INF#, 3)
            CALL DrawPixel(position, pixelColor, canvasConfig)
        NEXT y
    NEXT x
END SUB

SUB CanvasToViewPort (from2d AS CanvasPointType, to3d AS ViewportPointType, canvasConfig AS CanvasConfigType, viewportConfig AS ViewportConfigType)
    to3d.x = from2d.x * viewportConfig.width / canvasConfig.width
    to3d.y = from2d.y * viewportConfig.height / canvasConfig.height
    to3d.z = viewportConfig.distance
END SUB

FUNCTION TraceRay~& (spheres() AS SphereType, lights() AS LightType, origin AS ViewportPointType, rayDirection AS ViewportPointType, tMin AS DOUBLE, tMax AS DOUBLE, recursionDepth AS INTEGER)
    DIM closestIntersectionResult AS ClosestIntersectionReturnType
    CALL ClosestIntersection(spheres(), origin, rayDirection, tMin, tMax, closestIntersectionResult)

    DIM closestT AS DOUBLE
    DIM closestSphere AS SphereType
    closestT = closestIntersectionResult.closestT
    closestSphere = closestIntersectionResult.closestSphere

    IF closestSphere.radius = -1 THEN
        TraceRay~& = COLOR_WHITE~&
    ELSE
        DIM P AS ViewportPointType
        DIM normalVector AS ViewportPointType
        DIM V AS ViewportPointType

        'Compute local color
        DIM localColor AS _UNSIGNED LONG
        CALL ScalarMult3d(rayDirection, closestT, P)
        CALL Add3d(P, origin, P)
        CALL Difference3d(closestSphere.center, P, normalVector)
        CALL ScalarMult3d(normalVector, 1 / Length3d#(normalVector), normalVector)
        CALL ScalarMult3d(rayDirection, -1, V)
        lightIntensity# = ComputeLighting!(P, normalVector, V, spheres(), lights(), closestSphere.specular)
        localColor = ScalarMultColor~&(closestSphere.color, lightIntensity#)

        'Max recursion depth or not reflective?
        DIM reflective AS DOUBLE
        reflective = closestSphere.reflective
        IF recursionDepth <= 0 OR reflective <= 0 THEN
            TraceRay~& = localColor
        ELSE
            DIM reflectedRay AS ViewportPointType
            DIM negativeRayDirection AS ViewportPointType
            CALL ScalarMult3d(rayDirection, -1, negativeRayDirection)
            CALL ReflectRay(negativeRayDirection, normalVector, reflectedRay)
            DIM reflectedColor AS _UNSIGNED LONG
            reflectedColor = TraceRay~&(spheres(), lights(), P, reflectedRay, 0.001, MATH_INF#, recursionDepth - 1)
            TraceRay~& = AddColors~&(ScalarMultColor~&(localColor, 1 - reflective), ScalarMultColor~&(reflectedColor, reflective))
        END IF

    END IF
END FUNCTION

SUB ClosestIntersection (spheres() AS SphereType, origin AS ViewportPointType, rayDirection AS ViewportPointType, tMin AS DOUBLE, tMax AS DOUBLE, result AS ClosestIntersectionReturnType)
    DIM closestT AS DOUBLE
    closestT = MATH_INF#
    DIM closestSphere AS SphereType
    closestSphere.radius = -1

    DIM sphere AS SphereType
    DIM sphereIntersection AS SphereIntersectionType
    DIM t1 AS DOUBLE
    DIM t2 AS DOUBLE
    FOR i = 1 TO UBOUND(spheres)
        sphere = spheres(i)
        CALL IntersectRaySphere(origin, rayDirection, sphere, sphereIntersection)
        t1 = sphereIntersection.t1
        t2 = sphereIntersection.t2
        IF t1 < closestT AND t1 >= tMin AND t1 <= tMax THEN
            closestT = t1
            closestSphere = sphere
        END IF
        IF t2 < closestT AND t2 >= tMin AND t2 <= tMax THEN
            closestT = t2
            closestSphere = sphere
        END IF
    NEXT i

    result.closestSphere = closestSphere
    result.closestT = closestT
END SUB

SUB IntersectRaySphere (origin AS ViewportPointType, rayDirection AS ViewportPointType, sphere AS SphereType, sphereIntersection AS SphereIntersectionType)
    r = sphere.radius
    DIM CO AS ViewportPointType
    CALL Difference3d(sphere.center, origin, CO)
    DIM a, b, c, discriminant AS DOUBLE
    a = DotProduct3d#(rayDirection, rayDirection)
    b = 2 * DotProduct3d#(CO, rayDirection)
    c = DotProduct3d#(CO, CO) - r * r

    discriminant = b * b - 4 * a * c
    IF discriminant < 0 THEN
        'Set intersections to infinity
        sphereIntersection.t1 = MATH_INF#
        sphereIntersection.t2 = MATH_INF#
    ELSE
        sphereIntersection.t1 = (-b + SQR(discriminant)) / (2 * a)
        sphereIntersection.t2 = (-b - SQR(discriminant)) / (2 * a)
    END IF
END SUB

FUNCTION ComputeLighting! (pt AS ViewportPointType, normalVector AS ViewportPointType, V AS ViewportPointType, spheres() AS SphereType, lights() AS LightType, specular AS DOUBLE)
    DIM intensity AS SINGLE
    intensity = 0.0

    lenNormalVector# = Length3d#(normalVector)

    DIM L AS ViewportPointType
    DIM R AS ViewportPointType

    DIM tMax AS DOUBLE
    DIM closestIntersectionResult AS ClosestIntersectionReturnType
    DIM shadowSphere AS SphereType

    DIM light AS LightType
    FOR i = 1 TO UBOUND(lights)
        light = lights(i)
        IF light.source = "Ambient" THEN
            intensity = intensity + light.intensity
        ELSE
            IF light.source = "Point" THEN
                CALL Difference3d(pt, light.position, L)
                tMax = 1
            ELSE
                'light.source = "Directional"
                L = light.direction
                tMax = MATH_INF#
            END IF

            'Shadow check
            CALL ClosestIntersection(spheres(), pt, L, 0.001, tMax, closestIntersectionResult)
            shadowSphere = closestIntersectionResult.closestSphere
            IF shadowSphere.radius <> -1 THEN
                _CONTINUE
            END IF

            'Diffuse
            dot# = DotProduct3d#(normalVector, L)
            IF dot# > 0 THEN
                intensity = intensity + light.intensity * dot# / (lenNormalVector# * Length3d#(L))
            END IF

            'Specular
            IF specular >= 0 THEN
                CALL ReflectRay(L, normalVector, R)
                rDotV# = DotProduct3d#(R, V)
                IF rDotV# > 0 THEN
                    intensity = intensity + light.intensity * (rDotV# / (Length3d#(R) * Length3d#(V))) ^ specular
                END IF
            END IF
        END IF
    NEXT i

    ComputeLighting! = intensity
END FUNCTION

SUB ReflectRay (ray AS ViewportPointType, normalVector AS ViewportPointType, result AS ViewportPointType)
    CALL ScalarMult3d(normalVector, 2 * DotProduct3d#(normalVector, ray), result)
    CALL Difference3d(ray, result, result)
END SUB

SUB Add3d (vector AS ViewportPointType, pt AS ViewportPointType, resultVector AS ViewportPointType)
    resultVector.x = vector.x + pt.x
    resultVector.y = vector.y + pt.y
    resultVector.z = vector.z + pt.z
END SUB

SUB ScalarMult3d (vector AS ViewportPointType, scalar AS DOUBLE, resultVector AS ViewportPointType)
    resultVector.x = vector.x * scalar
    resultVector.y = vector.y * scalar
    resultVector.z = vector.z * scalar
END SUB

SUB Difference3d (position1 AS ViewportPointType, position2 AS ViewportPointType, resultingVector AS ViewportPointType)
    resultingVector.x = position2.x - position1.x
    resultingVector.y = position2.y - position1.y
    resultingVector.z = position2.z - position1.z
END SUB

FUNCTION Length3d# (vector AS ViewportPointType)
    Length3d# = SQR(DotProduct3d#(vector, vector))
END FUNCTION

FUNCTION DotProduct3d# (vector1 AS ViewportPointType, vector2 AS ViewportPointType)
    DotProduct3d# = vector1.x * vector2.x + vector1.y * vector2.y + vector1.z * vector2.z
END FUNCTION

FUNCTION TestScalarMultColor ()
    givenColor~& = COLOR_WHITE~&
    givenIntensity = 0.1

    actualColor~& = ScalarMultColor~&(givenColor~&, givenIntensity)

    actualRed% = _RED32(actualColor~&)
    expectedRed = 25
    IF actualRed% <> expectedRed THEN
        PRINT "TestScalarMultColor - Red was"; actualRed%; "instead of"; expectedRed
        TestScalarMultColor = -1
        EXIT FUNCTION
    END IF

    actualGreen% = _GREEN32(actualColor~&)
    expectedGreen = 25
    IF actualGreen% <> expectedGreen THEN
        PRINT "TestScalarMultColor - Green was"; actualGreen%; "instead of"; expectedGreen
        TestScalarMultColor = -1
        EXIT FUNCTION
    END IF

    actualBlue% = _BLUE32(actualColor~&)
    expectedBlue = 25
    IF actualBlue% <> expectedBlue THEN
        PRINT "TestScalarMultColor - Blue was"; actualBlue%; "instead of"; expectedBlue
        TestScalarMultColor = -1
        EXIT FUNCTION
    END IF

    TestScalarMultColor = 0
END FUNCTION

FUNCTION ScalarMultColor~& (clr AS _UNSIGNED LONG, intensity AS DOUBLE)
    red = _RED32(clr)
    green = _GREEN32(clr)
    blue = _BLUE32(clr)

    intensity = Max#(intensity, 0)

    red = INT(Min#(red * intensity, 255))
    green = INT(Min#(green * intensity, 255))
    blue = INT(Min#(blue * intensity, 255))

    ScalarMultColor~& = _RGB32(red, green, blue)
END FUNCTION

FUNCTION AddColors~& (color1 AS _UNSIGNED LONG, color2 AS _UNSIGNED LONG)
    red1 = _RED32(color1)
    green1 = _GREEN32(color1)
    blue1 = _BLUE32(color1)

    red2 = _RED32(color2)
    green2 = _GREEN32(color2)
    blue2 = _BLUE32(color2)

    red = Min#(red1 + red2, 255)
    green = Min#(green1 + green2, 255)
    blue = Min#(blue1 + blue2, 255)

    AddColors~& = _RGB32(red, green, blue)
END FUNCTION


SUB DrawPixel (position AS CanvasPointType, fgColor AS _UNSIGNED LONG, config AS CanvasConfigType)
    x = config.halfWidth + position.x
    y = config.halfHeight - position.y - 1
    IF x < 0 OR x > config.width OR y < 0 OR y > config.height THEN
        EXIT SUB
    END IF

    PSET (x, y), fgColor
END SUB

FUNCTION Max# (d1 AS DOUBLE, d2 AS DOUBLE)
    IF d1 >= d2 THEN
        Max# = d1
    ELSE
        Max# = d2
    END IF
END FUNCTION

FUNCTION Min# (d1 AS DOUBLE, d2 AS DOUBLE)
    IF d1 <= d2 THEN
        Min# = d1
    ELSE
        Min# = d2
    END IF
END FUNCTION

