"""Read-only OpenSlide tile adapter; invoked with explicit paths by R."""
import json
import sys


def main():
    import openslide
    from openslide.deepzoom import DeepZoomGenerator

    action, path = sys.argv[1:3]
    with openslide.OpenSlide(path) as slide:
        zoom = DeepZoomGenerator(slide, tile_size=254, overlap=1, limit_bounds=False)
        if action == "info":
            print(json.dumps({"width": slide.dimensions[0], "height": slide.dimensions[1],
                              "max_level": zoom.level_count - 1,
                              "vendor": slide.properties.get("openslide.vendor", "unknown")}))
        elif action == "tile":
            level, x, y = map(int, sys.argv[3:6])
            if not 0 <= level < zoom.level_count:
                raise ValueError("Invalid tile level")
            cols, rows = zoom.level_tiles[level]
            if not (0 <= x < cols and 0 <= y < rows):
                raise ValueError("Invalid tile coordinates")
            zoom.get_tile(level, (x, y)).save(sys.argv[6], "PNG")
        else:
            raise ValueError("Unknown slide operation")


if __name__ == "__main__":
    main()
