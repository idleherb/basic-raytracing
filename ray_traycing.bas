CONST MATH_INF = 10000


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
    width AS INTEGER
    height AS INTEGER
    distance AS INTEGER
    origin AS ViewportPointType
END TYPE


TYPE SphereType
    center AS ViewportPointType
    radius AS DOUBLE
    color AS _UNSIGNED LONG
END TYPE


TYPE SphereIntersectionType
    t1 AS DOUBLE
    t2 AS DOUBLE
END TYPE


DIM sphere1 AS SphereType
DIM sphere2 AS SphereType
DIM sphere3 AS SphereType
DIM center AS ViewportPointType
DIM spheres(1 TO 3) AS SphereType
'Sphere 1
center.x = 0
center.y = -1
center.z = 3
sphere1.center = center
sphere1.radius = 1
sphere1.color = _RGB32(128, 0, 128)
spheres(1) = sphere1

'Sphere 2
center.x = 2
center.y = 0
center.z = 4
sphere2.center = center
sphere2.radius = 1
sphere2.color = _RGB32(128, 128, 0)
spheres(2) = sphere2

'Sphere 3
center.x = -2
center.y = 0
center.z = 4
sphere3.center = center
sphere3.radius = 1
sphere3.color = _RGB32(0, 128, 128)
spheres(3) = sphere3


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
viewportConfig.width = 1
viewportConfig.height = 1
viewportConfig.distance = 1
viewportConfig.origin = cameraOrigin


CALL Main(spheres(), canvasConfig, viewportConfig)

SUB Main (spheres() AS SphereType, canvasConfig AS CanvasConfigType, viewportConfig AS ViewportConfigType)
    CALL InitCanvas(canvasConfig)
    CALL Render(spheres(), canvasConfig, viewportConfig)
END SUB

SUB InitCanvas (config AS CanvasConfigType)
    CLS
    SCREEN _NEWIMAGE(config.width, config.height, config.numColors)
END SUB

SUB Render (spheres() AS SphereType, canvasConfig AS CanvasConfigType, viewportConfig AS ViewportConfigType)
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
            pixelColor = TraceRay~&(spheres(), cameraOrigin, rayDirection, 1, MATH_INF)

            'IF pixelColor <> 4278190080 THEN
            '    CLS
            '    PRINT x
            '    PRINT y
            '    PRINT pixelColor
            '    DO
            '    LOOP UNTIL INKEY$ <> ""
            'END IF
            CALL DrawPixel(position, pixelColor, canvasConfig)
        NEXT y
    NEXT x
END SUB

SUB CanvasToViewPort (from2d AS CanvasPointType, to3d AS ViewportPointType, canvasConfig AS CanvasConfigType, viewportConfig AS ViewportConfigType)
    to3d.x = from2d.x * viewportConfig.width / canvasConfig.width
    to3d.y = from2d.y * viewportConfig.height / canvasConfig.height
    to3d.z = viewportConfig.distance
END SUB

FUNCTION TraceRay~& (spheres() AS SphereType, origin AS ViewportPointType, rayDirection AS ViewportPointType, tMin AS INTEGER, tMax AS INTEGER)
    DIM closestT AS DOUBLE
    closestT = MATH_INF
    DIM sphere AS SphereType
    DIM closestSphere AS SphereType
    closestSphere.radius = -1
    DIM sphereIntersection AS SphereIntersectionType
    DIM t1 AS DOUBLE
    DIM t2 AS DOUBLE

    FOR i = 1 TO 3
        sphere = spheres(i)
        CALL IntersectRaySphere(origin, rayDirection, sphere, sphereIntersection)
        t1 = sphereIntersection.t1
        t2 = sphereIntersection.t2
        IF t1 >= tMin AND t1 <= tMax AND t1 < closestT THEN
            closestT = t1
            closestSphere = sphere
        END IF
        IF t2 >= tMin AND t2 <= tMax AND t2 < closestT THEN
            closestT = t2
            closestSphere = sphere
        END IF
    NEXT i

    IF closestSphere.radius = -1 THEN
        TraceRay~& = _RGB32(0, 0, 0)
    ELSE
        TraceRay~& = closestSphere.color
    END IF
END FUNCTION

SUB IntersectRaySphere (origin AS ViewportPointType, rayDirection AS ViewportPointType, sphere AS SphereType, sphereIntersection AS SphereIntersectionType)
    r = sphere.radius
    DIM CO AS ViewportPointType
    CALL Difference3d(sphere.center, origin, CO)
    DIM a, b, c, discriminant AS DOUBLE
    a = DotProduct3d(rayDirection, rayDirection)
    b = 2 * DotProduct3d(CO, rayDirection)
    c = DotProduct3d(CO, CO) - r * r

    discriminant = b * b - 4 * a * c
    IF discriminant < 0 THEN
        'Set intersections to infinity
        sphereIntersection.t1 = MATH_INF
        sphereIntersection.t2 = MATH_INF
    ELSE
        sphereIntersection.t1 = (-b + SQR(discriminant)) / (2 * a)
        sphereIntersection.t2 = (-b - SQR(discriminant)) / (2 * a)
    END IF
END SUB

SUB Difference3d (position1 AS ViewportPointType, position2 AS ViewportPointType, resultingVector AS ViewportPointType)
    resultingVector.x = position2.x - position1.x
    resultingVector.y = position2.y - position1.y
    resultingVector.z = position2.z - position1.z
END SUB

FUNCTION DotProduct3d (vector1 AS ViewportPointType, vector2 AS ViewportPointType)
    DotProduct3d = vector1.x * vector2.x + vector1.y * vector2.y + vector1.z * vector2.z
END FUNCTION

SUB DrawPixel (position AS CanvasPointType, fgColor AS _UNSIGNED LONG, config AS CanvasConfigType)
    PSET (config.halfWidth + position.x, config.halfHeight - position.y), fgColor
END SUB

