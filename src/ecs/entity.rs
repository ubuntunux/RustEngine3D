pub type EntityId = u64;

pub const INVALID_ENTITY_ID: EntityId = u64::MAX;
pub const INVALID_INDEX: usize = usize::MAX;

#[derive(Debug, Clone)]
pub struct ComponentStorage<T> {
    components: Vec<T>,
    entity_id_list: Vec<EntityId>,
    index_list: Vec<usize>,
    free_list: Vec<EntityId>,
}

impl<T> Default for ComponentStorage<T> {
    fn default() -> Self {
        Self {
            components: Vec::with_capacity(1024),
            entity_id_list: Vec::with_capacity(1024),
            index_list: Vec::with_capacity(1024),
            free_list: Vec::with_capacity(256),
        }
    }
}

impl<T> ComponentStorage<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register(&mut self, component: T) -> EntityId {
        let entity_id = if let Some(id) = self.free_list.pop() {
            id
        } else {
            self.index_list.len() as EntityId
        };

        self.register_with_id(entity_id, component);
        entity_id
    }

    pub fn register_with_id(&mut self, entity_id: EntityId, component: T) {
        let entity_idx = entity_id as usize;
        if entity_idx >= self.index_list.len() {
            self.index_list.resize(entity_idx + 1, INVALID_INDEX);
        }

        let comp_idx = self.index_list[entity_idx];
        if comp_idx != INVALID_INDEX {
            self.components[comp_idx] = component;
        } else {
            let new_comp_idx = self.components.len();
            self.index_list[entity_idx] = new_comp_idx;
            self.components.push(component);
            self.entity_id_list.push(entity_id);
        }
    }

    pub fn unregister(&mut self, entity_id: EntityId) -> Option<T> {
        let entity_idx = entity_id as usize;
        if entity_idx >= self.index_list.len() {
            return None;
        }

        let comp_idx = self.index_list[entity_idx];
        if comp_idx == INVALID_INDEX {
            return None;
        }

        let last_idx = self.components.len() - 1;

        let removed_component = self.components.swap_remove(comp_idx);
        let removed_entity = self.entity_id_list.swap_remove(comp_idx);

        if comp_idx != last_idx {
            let moved_entity = self.entity_id_list[comp_idx];
            self.index_list[moved_entity as usize] = comp_idx;
        }

        self.index_list[removed_entity as usize] = INVALID_INDEX;
        self.free_list.push(removed_entity);

        Some(removed_component)
    }

    #[inline(always)]
    pub fn get(&self, entity_id: EntityId) -> Option<&T> {
        let entity_idx = entity_id as usize;
        if entity_idx < self.index_list.len() {
            let comp_idx = self.index_list[entity_idx];
            if comp_idx != INVALID_INDEX {
                return unsafe { Some(self.components.get_unchecked(comp_idx)) };
            }
        }
        None
    }

    #[inline(always)]
    pub fn get_mut(&mut self, entity_id: EntityId) -> Option<&mut T> {
        let entity_idx = entity_id as usize;
        if entity_idx < self.index_list.len() {
            let comp_idx = self.index_list[entity_idx];
            if comp_idx != INVALID_INDEX {
                return unsafe { Some(self.components.get_unchecked_mut(comp_idx)) };
            }
        }
        None
    }

    pub fn len(&self) -> usize {
        self.components.len()
    }

    pub fn is_empty(&self) -> bool {
        self.components.is_empty()
    }

    #[inline(always)]
    pub fn dense_slice(&self) -> &[T] {
        &self.components
    }

    #[inline(always)]
    pub fn dense_slice_mut(&mut self) -> &mut [T] {
        &mut self.components
    }

    pub fn iter(&self) -> impl Iterator<Item = (EntityId, &T)> {
        self.entity_id_list.iter().copied().zip(self.components.iter())
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (EntityId, &mut T)> {
        self.entity_id_list.iter().copied().zip(self.components.iter_mut())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_component_storage_refactored() {
        let mut storage = ComponentStorage::<String>::new();

        let id0 = storage.register("Entity 0".to_string());
        let id1 = storage.register("Entity 1".to_string());
        let id2 = storage.register("Entity 2".to_string());

        assert_eq!(id0, 0);
        assert_eq!(id1, 1);
        assert_eq!(id2, 2);
        assert_eq!(storage.len(), 3);

        let removed = storage.unregister(id1);
        assert_eq!(removed, Some("Entity 1".to_string()));
        assert_eq!(storage.get(id1), None);
        assert_eq!(storage.len(), 2);

        let id3 = storage.register("Entity 3 (Reused ID 1)".to_string());
        assert_eq!(id3, id1);
        assert_eq!(storage.get(id3), Some(&"Entity 3 (Reused ID 1)".to_string()));
        assert_eq!(storage.len(), 3);
    }
}
