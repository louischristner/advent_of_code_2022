#include <vector>
#include <fstream>
#include <iostream>
#include <algorithm>

struct Tree {
    Tree(std::size_t height, int x, int y):
        height(height), x(x), y(y) {}

    std::size_t height;
    int x, y;
};

bool isTreeVisible(
    const std::vector<std::string> &trees,
    const int i,
    const int j,
    const std::size_t height,
    const std::size_t width
) {
    int hiddingTrees = 0;

    for (int i_t = i - 1; i_t >= 0; i_t--) {
        if (trees[i_t][j] >= trees[i][j]) {
            hiddingTrees++;
            break;
        }
    }

    for (int i_b = i + 1; i_b < height; i_b++) {
        if (trees[i_b][j] >= trees[i][j]) {
            hiddingTrees++;
            break;
        }
    }

    for (int j_l = j - 1; j_l >= 0; j_l--) {
        if (trees[i][j_l] >= trees[i][j]) {
            hiddingTrees++;
            break;
        }
    }

    for (int j_r = j + 1; j_r < width; j_r++) {
        if (trees[i][j_r] >= trees[i][j]) {
            hiddingTrees++;
            break;
        }
    }

    return hiddingTrees < 4;
}

int getTreeVisibility(
    const std::vector<std::string> &trees,
    const int i,
    const int j,
    const std::size_t height,
    const std::size_t width
) {
    int topVisibleTrees = 0;
    int leftVisibleTrees = 0;
    int rightVisibleTrees = 0;
    int bottomVisibleTrees = 0;

    for (int i_t = i - 1; i_t >= 0; i_t--) {
        topVisibleTrees++;
        if (trees[i_t][j] >= trees[i][j])
            break;
    }

    for (int i_b = i + 1; i_b < height; i_b++) {
        bottomVisibleTrees++;
        if (trees[i_b][j] >= trees[i][j])
            break;
    }

    for (int j_l = j - 1; j_l >= 0; j_l--) {
        leftVisibleTrees++;
        if (trees[i][j_l] >= trees[i][j])
            break;
    }

    for (int j_r = j + 1; j_r < width; j_r++) {
        rightVisibleTrees++;
        if (trees[i][j_r] >= trees[i][j])
            break;
    }

    return (
        topVisibleTrees *
        leftVisibleTrees *
        rightVisibleTrees *
        bottomVisibleTrees
    );
}

int main(int ac, char **av)
{
    if (ac <= 1)
        return 0;

    std::string filePath = av[1];
    std::cout << filePath << std::endl;

    std::ifstream file(filePath);
    std::vector<std::string> trees;

    for (std::string input; std::getline(file, input); )
        trees.push_back(input);

    std::size_t height = trees.size();
    std::size_t width = trees[0].size();

    std::vector<Tree> visibleTrees;
    std::vector<int> visibilityOfTrees;

    for (int i = 1; i < height - 1; i++) {
        for (int j = 1; j < height - 1; j++) {
            if (isTreeVisible(trees, i, j, height, width)) {
                visibleTrees.push_back(Tree('0' - trees[i][j], j, i));
                visibilityOfTrees.push_back(getTreeVisibility(trees, i, j, height, width));
            }
        }
    }

    std::cout << (height + width) * 2 - 4 + visibleTrees.size() << std::endl;
    std::cout << *std::max_element(std::begin(visibilityOfTrees), std::end(visibilityOfTrees)) << std::endl;

    return 0;
}