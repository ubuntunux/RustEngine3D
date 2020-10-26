use serde::{ Serialize, Deserialize };
use nalgebra;
use nalgebra::{ Vector3 };

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct BoundingBox {
    pub _min: Vector3<f32>,
    pub _max: Vector3<f32>,
    pub _center: Vector3<f32>,
    pub _radius: f32
}

impl Default for BoundingBox {
    fn default() -> BoundingBox {
        let min = Vector3::new(-1.0, -1.0, -1.0);
        let max = Vector3::new(1.0, 1.0, 1.0);
        BoundingBox {
            _min: min.clone(),
            _max: max.clone(),
            _center: (max + min) * 0.5,
            _radius: (max - min).norm() * 0.5
        }
    }
}


pub fn calc_bounding_box(positions: &Vec<Vector3<f32>>) -> BoundingBox {
    if 0 == positions.len() {
        return BoundingBox::default();
    }

    BoundingBox::default()
    //     let (minValue, maxValue) = calcBoundingBox' position position positions
    //     S radius = normL2 $ (maxValue - minValue)
    //     in BoundingBox
    //     { _bounding_box_min = minValue
    //         , _bounding_box_max = maxValue
    //         , _bounding_box_center = (minValue + maxValue) * 0.5
    //     , _bounding_box_radius = radius
    // }
    // where
    // calcBoundingBox' :: Vector3<f32> -> Vector3<f32> -> [Vector3<f32>] -> (Vector3<f32>, Vector3<f32>)
    // calcBoundingBox' boundMin boundMax [] = (boundMin, boundMax)
    // calcBoundingBox' boundMin boundMax (position:positions) =
    // let (# minX, minY, minZ #) = unpackV3# boundMin
    //     (# maxX, maxY, maxZ #) = unpackV3# boundMax
    // minValue = vec3 (min minX maxX) (min minY maxY) (min minZ maxZ)
    // maxValue = vec3 (max minX maxX) (max minY maxY) (max minZ maxZ)
    // in
    // calcBoundingBox' minValue maxValue positions
}

